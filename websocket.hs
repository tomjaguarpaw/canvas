{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

import qualified Data.Text.Lazy     as T
import qualified Network.WebSockets as WS
import           Data.Monoid        ((<>))
import qualified Data.IORef         as R
import qualified Text.Blaze         as B
import           Text.Blaze.Html5   ((!))
import qualified Text.Blaze.Svg11   as S
import qualified Text.Blaze.Svg11.Attributes as AS
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Control.Lens       as L
import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty((:|)))

data CircleEvent = MouseOver | MouseOut | MouseClick deriving Show
data CircleState = CircleState { _csHovered  :: Bool
                               , _csSelected :: Bool } deriving Show

data GUICircle = GUICircle { gcName  :: T.Text
                           , gcColor :: T.Text } deriving Show

data Circle = Circle { _cName  :: T.Text
                     , _cState :: CircleState } deriving Show
$(L.makeLenses ''Circle)
$(L.makeLenses ''CircleState)

type Message = T.Text

data Canvas a = Canvas [GUICircle] (Message -> Maybe a)

nullCanvas :: Canvas a
nullCanvas = Canvas [] (const Nothing)

instance Functor Canvas where
  fmap f (Canvas cs h) = Canvas cs ((fmap . fmap) f h)

circleMake :: T.Text -> Circle
circleMake n = Circle n (CircleState False False)

circleHandle :: CircleEvent -> Circle -> Circle
circleHandle MouseOver  = L.set  (cState.csHovered)  True
circleHandle MouseOut   = L.set  (cState.csHovered)  False
circleHandle MouseClick = L.over (cState.csSelected) not

guiCircle :: Circle -> GUICircle
guiCircle c = GUICircle { gcName  = _cName c
                        , gcColor = case (L.view (cState.csHovered) c, L.view (cState.csSelected) c)
                                    of (True, True)   -> "#cc0000"
                                       (True, False)  -> "#cccccc"
                                       (False, True)  -> "#ff0000"
                                       (False, False) -> "#ffffff" }

circle :: Circle -> Canvas CircleEvent
circle c@(Circle name _) = Canvas [guiCircle c]
                                  (\message -> case T.split (== ',') message
                                               of [theName, theEvent] ->
                                                    if theName == name
                                                    then case theEvent
                                                         of "mouseover" -> Just MouseOver
                                                            "mouseout"  -> Just MouseOut
                                                            "click"     -> Just MouseClick
                                                            _           -> Nothing
                                                    else Nothing
                                                  _ -> Nothing)

horiz :: Canvas a -> Canvas a -> Canvas a
horiz (Canvas xs xh) (Canvas ys yh) = Canvas (xs ++ ys)
                                             (\message -> case xh message of
                                                 r@(Just _) -> r
                                                 Nothing -> case yh message of
                                                   s@(Just _) -> s
                                                   Nothing -> Nothing)                                                          

handleMessage :: Canvas a -> Message -> Maybe a
handleMessage (Canvas _ h) = h

render :: Canvas a -> T.Text
render (Canvas cs _) = renderHtml $ S.svg ! AS.width (B.toValue (100 * length cs))
                                          ! AS.height "100" $ do
  sequence_ (package cs [0..])
  
  where package = zipWith (\c i -> circleSvg (50 + i * 100) 50 (B.toValue (gcColor c)) (B.toValue (gcName c)))

circleSvg :: Int -> Int -> S.AttributeValue -> S.AttributeValue -> S.Svg
circleSvg cx cy color name = 
  S.circle ! AS.cx (B.toValue cx)
           ! AS.cy (B.toValue cy)
           ! AS.r "40"
           ! AS.stroke "black"
           ! AS.strokeWidth "4"
           ! AS.fill color
           ! AS.onmouseover ("mouseover('" <> name <> "')")
           ! AS.onmouseout  ("mouseout('" <> name <> "')")
           ! AS.onclick     ("click('" <> name <> "')")


ne :: NEL.NonEmpty a -> Either a (a, NEL.NonEmpty a)
ne (a :| []) = Left a
ne (a :| (a':as)) = Right (a, a' :| as)

singleton :: a -> NonEmpty a
singleton a = a :| []


data Package e b = Package { _pState :: b, _pRender :: Canvas (e, b, Package e b) }
-- $(L.makeLenses ''Package)

instance Functor (Package e) where
  fmap f p = Package { _pState  = f (_pState p)
                     , _pRender = fmap (\(ev, b, p) -> (ev, f b, fmap f p)) (_pRender p) }

horizP :: Package e a -> Package e b -> Package e (a, b)
horizP p1 p2 = Package { _pState = (_pState p1, _pState p2)
                       , _pRender = fmap (\(ev, b, peb) -> (ev, (b, _pState p2), peb `horizP` p2)) (_pRender p1)
                                    `horiz`
                                    fmap (\(ev, b, peb) -> (ev, (_pState p1, b), p1 `horizP` peb)) (_pRender p2) }


traverseNEL :: Functor f =>
               (forall a b. f a -> f b -> f (a, b))
               -> NEL.NonEmpty (f c)
               -> f (NEL.NonEmpty c)
traverseNEL (***) l = case ne l of               
  Left fa         -> fmap singleton fa
  Right (fa, fas) -> fmap (uncurry NEL.cons) (fa *** traverseNEL (***) fas)

data Selected = Selected Circle

data Unselected = Unselected Circle

circlePackage' :: Circle -> Package CircleEvent Circle
circlePackage' c = Package { _pState = c
                           , _pRender = fmap (\ev -> (ev, (circleHandle ev c), circlePackage' (circleHandle ev c))) (circle c) }

circlePackage :: T.Text -> Package CircleEvent Circle
circlePackage = circlePackage' . circleMake

selectedC :: Selected -> Canvas (CircleEvent, Selected)
selectedC s@(Selected c) = fmap (\ev -> let new = selectedHandle ev s
                                        in (ev, new)) (circle c)

unselectedC :: Unselected -> Canvas (CircleEvent, Unselected)
unselectedC s@(Unselected c) = fmap (\ev -> let new = unselectedHandle ev s
                                           in (ev, new)) (circle c)

selected' :: Selected -> Package CircleEvent Selected
selected' s = Package { _pState  = s
                      , _pRender = fmap (\(ev, new) -> (ev, new, selected' new)) (selectedC s) }

selected :: T.Text -> Package CircleEvent Selected
selected = selected' . Selected . circleMake

unselectedHandle :: CircleEvent -> Unselected -> Unselected
unselectedHandle ev (Unselected c) = Unselected ((case ev of MouseClick -> id
                                                             _          -> circleHandle ev) c)

selectedHandle :: CircleEvent -> Selected -> Selected
selectedHandle ev (Selected c) = Selected ((case ev of MouseClick -> id
                                                       _          -> circleHandle ev) c)


unselected' :: Unselected -> Package CircleEvent Unselected
unselected' u = Package { _pState  = u
                        , _pRender = fmap (\(ev, new) -> (ev, new, unselected' new)) (unselectedC u) }

unselected :: T.Text -> Package CircleEvent Unselected
unselected = unselected' . Unselected . L.set (cState.csSelected) True . circleMake

data Radio x o = Chosen x [o] | Unchosen o (Radio x o)

traverseRadio :: Functor f =>
                 (forall a b. f a -> f b -> f (a, b))
              -> Radio (f x) (f o)
              -> f (Radio x o)
traverseRadio (***) l = case l of
  Chosen fx []       -> fmap (\x -> Chosen x []) fx
  Chosen fx (fy:fys) -> fmap (\(x, ys) -> Chosen x (NEL.toList ys)) (fx *** traverseNEL (***) (fy :| fys))
  Unchosen fo fas    -> fmap (\(o, as) -> Unchosen o as) (fo *** traverseRadio (***) fas)

canvasUnselected :: NEL.NonEmpty Unselected
                   -> Canvas (CircleEvent, Either (NEL.NonEmpty Unselected) (Radio Selected Unselected))
canvasUnselected l = case ne l of
  Left p -> fmap (\ev -> (ev, case ev of
                             MouseClick -> Right (Chosen (Selected (L.set (cState.csSelected) True c)) [])
                             e          -> Left (singleton (unselectedHandle e u)))) (circle c)
    where u@(Unselected c) = p

  Right (p, ps) -> fmap (\ev -> (ev, (case ev of
                                         MouseClick -> Right (Chosen (Selected (L.set (cState.csSelected) True c))
                                                                     (NEL.toList ps))
                                         e          -> Left (unselectedHandle e u
                                                             `NEL.cons` ps)
                                                             ))) (circle c)
                   `horiz`
                   fmap (\(ev, rest) -> (ev, case rest of
                                            Left r -> Left (p `NEL.cons` r)
                                            Right r -> Right (Unchosen p r)
                                        )) (canvasUnselected ps)


    where u@(Unselected c) = p


canvasRadio :: Radio Selected Unselected
            -> Canvas (CircleEvent, Radio Selected Unselected)
canvasRadio l = case l of
  Chosen s []     -> fmap (\(ev, s') -> (ev, Chosen s' [])) (selectedC s)
  Chosen s (y:ys) -> fmap (\(ev, s') -> (ev, Chosen s' ylis)) (selectedC s)
                      `horiz`
                      fmap (\(ev, y') -> (ev, case y' of
                               Left u -> Chosen s (NEL.toList u)
                               Right ss -> Unchosen (Unselected (L.set (cState.csSelected) False c)) ss))
                           (canvasUnselected ynel)
    where ynel = y :| ys
          ylis = y : ys
          Selected c = s
            

  Unchosen u r     -> fmap (\(ev, s') -> (ev, case ev of
                                             MouseClick -> Chosen (Selected (L.set (cState.csSelected) True c))
                                                                  (radioToList
                                                                   (fmapRadio (\(Selected s) ->
                                                                                Unselected
                                                                                (L.set (cState.csSelected) False s))
                                                                                id r))
                                             _          -> Unchosen (unselectedHandle ev u) r))
                           (unselectedC u)
                      `horiz`
                      fmap (\(ev, s') -> (ev, Unchosen u s')) (canvasRadio r)


    where Unselected c = u



makePackage :: (a -> Canvas (ev, a)) -> a -> Package ev a
makePackage f a = Package { _pState = a
                          , _pRender = fmap (\(ev, a) -> (ev, a, makePackage f a)) (f a) }

--  Unchosen fo fas    -> fmap (\(o, as) -> Unchosen o as) (fo *** traverseRadio (***) fas)

fmapRadio :: (x -> x') -> (o -> o') -> Radio x o -> Radio x' o'
fmapRadio f g (Chosen x os) = Chosen (f x) (fmap g os)
fmapRadio f g (Unchosen o rs) = Unchosen (g o) (fmapRadio f g rs)

radioToList :: Radio a a -> [a]
radioToList (Chosen x xs) = x : xs
radioToList (Unchosen x xs) = x : radioToList xs


meow :: R.IORef Int -> WS.PendingConnection -> IO ()
meow r pc = do
  conn <- WS.acceptRequest pc

--  let initialGui = circlePackage "id1" `horizP` circlePackage "id2"
{-
  let initialGui = traverseNEL horizP (circlePackage "id1" :| [circlePackage "id2", circlePackage "id3"])
                   `horizP`
                   unselected "id4"
                   `horizP`
                   selected "id5"
-}

  let initialGui = makePackage canvasRadio (Chosen (Selected (L.set (cState.csSelected) True (circleMake "id1")))
                                                   [ Unselected (circleMake "id2")
                                                   , Unselected (circleMake "id3")
                                                   , Unselected (circleMake "id4") ])
                                                       

  let loop gui = do
        msg  <- WS.receiveData conn
        n    <- R.readIORef r
        
        let mNextGui = handleMessage (_pRender gui) msg
            nextGui = maybe gui (L.view L._3) mNextGui
  
        print msg
--        print mNextGui

        R.writeIORef r (n + 1)
        WS.sendTextData conn (render (_pRender nextGui))
  
        loop nextGui

  loop initialGui


main :: IO ()
main = do
  r <- R.newIORef 0
  WS.runServer "0.0.0.0" 9998 (meow r)
