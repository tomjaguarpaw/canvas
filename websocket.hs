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


data Package e b = Package { _pState :: b, _pRender :: Canvas (e, Package e b) }
-- $(L.makeLenses ''Package)

instance Functor (Package e) where
  fmap f p = Package { _pState  = f (_pState p)
                     , _pRender = fmap (L.over L._2 (fmap f)) (_pRender p) }

horizP :: Package e a -> Package e b -> Package e (a, b)
horizP p1 p2 = Package { _pState = (_pState p1, _pState p2)
                       , _pRender = fmap (\(ev, peb) -> (ev, peb `horizP` p2)) (_pRender p1)
                                    `horiz`
                                    fmap (\(ev, peb) -> (ev, p1 `horizP` peb)) (_pRender p2) }


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
                           , _pRender = fmap (\ev -> (ev, circlePackage' (circleHandle ev c))) (circle c) }

circlePackage :: T.Text -> Package CircleEvent Circle
circlePackage = circlePackage' . circleMake

selected' :: Selected -> Package CircleEvent Selected
selected' (Selected c) = Package { _pState  = Selected c
                                 , _pRender = fmap (\ev -> (ev, selected' (Selected ((case ev of MouseClick -> id
                                                                                                 _          -> circleHandle ev)
                                                                                     c)))) (circle c) }

selected :: T.Text -> Package CircleEvent Selected
selected = selected' . Selected . circleMake

unselected' :: Unselected -> Package CircleEvent Unselected
unselected' (Unselected c) = Package { _pState  = Unselected c
                                     , _pRender = fmap (\ev -> (ev,
  unselected' (Unselected ((case ev of MouseClick -> id
                                       _          -> circleHandle ev)
                           c)))) (circle c) }

unselected :: T.Text -> Package CircleEvent Unselected
unselected = unselected' . Unselected . L.set (cState.csSelected) True . circleMake

meow :: R.IORef Int -> WS.PendingConnection -> IO ()
meow r pc = do
  conn <- WS.acceptRequest pc

--  let initialGui = circlePackage "id1" `horizP` circlePackage "id2"
  let initialGui = traverseNEL horizP (circlePackage "id1" :| [circlePackage "id2", circlePackage "id3"])
                   `horizP`
                   unselected "id4"
                   `horizP`
                   selected "id5"

  let loop gui = do
        msg  <- WS.receiveData conn
        n    <- R.readIORef r
        
        let mNextGui = handleMessage (_pRender gui) msg
            nextGui = maybe gui snd mNextGui
  
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
