{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Text.Lazy     as T
import qualified Network.WebSockets as WS
import           Data.Monoid        ((<>))
import qualified Text.Blaze         as B
import           Text.Blaze.Html5   ((!))
import qualified Text.Blaze.Svg11   as S
import qualified Text.Blaze.Svg11.Attributes as AS
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Control.Lens       as L
import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Control.Arrow      ((>>>))

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
                        , gcColor = (circleColor . L.view cState) c }

circleColor :: CircleState -> T.Text
circleColor c = case (L.view csHovered c, L.view csSelected c)
                of (True, True)   -> "#cc0000"
                   (True, False)  -> "#cccccc"
                   (False, True)  -> "#ff0000"
                   (False, False) -> "#ffffff"

parseCircleEvent :: T.Text -> Maybe CircleEvent
parseCircleEvent = \case "mouseover" -> Just MouseOver
                         "mouseout"  -> Just MouseOut
                         "click"     -> Just MouseClick
                         _           -> Nothing

circle :: Circle -> Canvas CircleEvent
circle c@(Circle name _) = Canvas [guiCircle c] parseMessage
  where parseMessage message = case T.split (== ',') message
                               of [theName, theEvent] ->
                                    if theName == name
                                    then parseCircleEvent theEvent
                                    else Nothing
                                  _ -> Nothing

circleC :: Circle -> Canvas (CircleEvent, Circle)
circleC c = fmap (\ev -> (ev, circleHandle ev c)) (circle c)

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
render (Canvas cs _) = renderHtml (documentSvg h w (sequence_ (package cs [0..])))

  where package = zipWith (\c i -> circleSvg (50 + i * 100) 50 (B.toValue (gcColor c)) (B.toValue (gcName c)))
        w = 100 * length cs
        h = 100

documentSvg :: Int -> Int -> S.Svg -> S.Svg
documentSvg h w = S.svg ! AS.width (B.toValue w)
                        ! AS.height (B.toValue h)

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
                     , _pRender = fmap (\(ev, b, pa) -> (ev, f b, fmap f pa)) (_pRender p) }

traverseNEL :: Functor f =>
               (forall a b. f a -> f b -> f (a, b))
               -> NEL.NonEmpty (f c)
               -> f (NEL.NonEmpty c)
traverseNEL (***) = ne >>> \case
  Left fa         -> fmap singleton fa
  Right (fa, fas) -> fmap (uncurry NEL.cons) (fa *** traverseNEL (***) fas)

data Selected = Selected Circle

data Unselected = Unselected Circle

selectedC :: Selected -> Canvas (CircleEvent, Selected)
selectedC s@(Selected c) = fmap (\ev -> let new = selectedHandle ev s
                                        in (ev, new)) (circle c)

unselectedC :: Unselected -> Canvas (CircleEvent, Unselected)
unselectedC s@(Unselected c) = fmap (\ev -> let new = unselectedHandle ev s
                                           in (ev, new)) (circle c)

unselectedHandle :: CircleEvent -> Unselected -> Unselected
unselectedHandle ev (Unselected c) = Unselected ((case ev of MouseClick -> id
                                                             _          -> circleHandle ev) c)

selectedHandle :: CircleEvent -> Selected -> Selected
selectedHandle ev (Selected c) = Selected ((case ev of MouseClick -> id
                                                       _          -> circleHandle ev) c)


selectedOfUnselected :: Unselected -> Selected
selectedOfUnselected (Unselected c) = Selected (L.set (cState.csSelected) True c)

unselectedOfSelected :: Selected -> Unselected
unselectedOfSelected (Selected c) = Unselected (L.set (cState.csSelected) False c)

data Radio x o = Chosen x [o]
               | Unchosen o (Radio x o)
               deriving Show

data Radio' x o = Chosen1' x
                | Chosen' x (NEL.NonEmpty o)
                | Unchosen' o (Radio x o)

data RadioO x o = Before (Radio x o) o [o]
                | After  [o] o (Radio x o)
                deriving Show

data RadioX x o = At [o] x [o]
                deriving Show

data NELZ a = NELZ [a] a [a]

consNELZ :: a -> NELZ a -> NELZ a
consNELZ u (NELZ ls a rs) = NELZ (u:ls) a rs

consRadioO :: o -> RadioO x o -> RadioO x o
consRadioO u (Before rs o os) = Before (consRadio u rs) o os
consRadioO u (After os o rs)  = After  (u:os) o rs

consRadioONEL :: x -> NELZ o -> RadioO x o
consRadioONEL x (NELZ ls a rs) = Before (Chosen x ls) a rs

consRadioX :: o -> RadioX x o -> RadioX x o
consRadioX o (At os x os') = At (o:os) x os'

consRadio :: o -> Radio x o -> Radio x o
consRadio o rs = Unchosen o rs

unchoose :: (x -> o) -> Radio x o -> NEL.NonEmpty o
unchoose f (Chosen x os)   = f x :| os
unchoose f (Unchosen o rs) = o `NEL.cons` unchoose f rs

toRadio' :: Radio x o -> Radio' x o
toRadio' (Chosen x [])     = Chosen1' x
toRadio' (Chosen x (y:ys)) = Chosen' x (y :| ys)
toRadio' (Unchosen x rs)   = Unchosen' x rs

fromRadio' :: Radio' x o -> Radio x o
fromRadio' (Chosen1' x)          = Chosen x []
fromRadio' (Chosen' x (y :| ys)) = Chosen x (y:ys)
fromRadio' (Unchosen' x rs)      = Unchosen x rs

stampX :: RadioX x o -> Radio x o
stampX (At [] x rs) = Chosen x rs
stampX (At (l:ls) x rs) = Unchosen l (stampX (At ls x rs))

appendO :: Radio x o -> [o] -> Radio x o
appendO (Chosen x os) os'   = Chosen x (os ++ os')
appendO (Unchosen o rs) os' = Unchosen o (appendO rs os')

prependO :: [o] -> Radio x o -> Radio x o
prependO [] rs     = rs
prependO (o:os) rs = Unchosen o (prependO os rs)

stampO :: RadioO x o -> Radio x o
stampO (Before rs o os) = rs `appendO` (o:os)
stampO (After os o rs)  = (os ++ [o]) `prependO` rs

traverseRadio :: Functor f =>
                 (forall a b. f a -> f b -> f (a, b))
              -> Radio (f x) (f o)
              -> f (Radio x o)
traverseRadio (***) = fmap fromRadio' . cases . toRadio'
  where cases = \case
          Chosen1'  fx     -> fmap (\x -> Chosen1' x) fx
          Chosen'   fx fys -> fmap (\(x, ys) -> Chosen' x ys)
                                   (fx *** traverseNEL (***) fys)
          Unchosen' fo fas -> fmap (\(o, as) -> Unchosen' o as)
                                   (fo *** traverseRadio (***) fas)

duplicateNEL :: NEL.NonEmpty a -> NEL.NonEmpty (NELZ a)
duplicateNEL = ne >>> \case
  Left a        -> singleton (NELZ [] a [])
  Right (a, as) -> NELZ [] a (NEL.toList as)
                   `NEL.cons` (fmap (a `consNELZ`) (duplicateNEL as))

duplicateRadio' :: Radio' x o -> Radio' (RadioX x o) (RadioO x o)
duplicateRadio' (Chosen1' x)     = Chosen1' (At [] x [])
duplicateRadio' (Chosen' x xs)   = Chosen' (At [] x (NEL.toList xs))
                                           (fmap (x `consRadioONEL`)
                                                 (duplicateNEL xs))
duplicateRadio' (Unchosen' o rs) = Unchosen' (After [] o rs) rost
  where rest = fromRadio' (duplicateRadio' (toRadio' rs))
        rost = fmapRadio (o `consRadioX`) (o `consRadioO`) rest

duplicateRadio :: Radio x o -> Radio (RadioX x o) (RadioO x o)
duplicateRadio = fromRadio' . duplicateRadio' . toRadio'

canvasRadioX :: RadioX Selected Unselected
             -> Canvas (CircleEvent, Radio Selected Unselected)
canvasRadioX (At ls x rs) = fmap (\(ev, x') -> (ev, stampX (At ls x' rs)))
                                 (selectedC x)

canvasRadioO :: RadioO Selected Unselected
             -> Canvas (CircleEvent, Radio Selected Unselected)
canvasRadioO = \case
  (Before rs o os) -> fmap (\(ev, n) -> (ev, case ev of
                                            MouseClick ->
                                              stampX (At
                                                (NEL.toList (radioToNEL (fmapRadio unselectedOfSelected id rs)))
                                                (selectedOfUnselected n)
                                                os)
                                            _ ->
                                              stampO (Before rs n os)
                                        ))
                           (unselectedC o)
  (After os o rs) -> fmap (\(ev, n) -> (ev, case ev of
                                           MouseClick ->
                                             stampX (At
                                               os
                                               (selectedOfUnselected n)
                                               (NEL.toList (radioToNEL (fmapRadio unselectedOfSelected id rs))))
                                           _ ->
                                             stampO (After os n rs)
                                       ))
                           (unselectedC o)


canvasRadio :: Radio Selected Unselected -> Canvas (CircleEvent, Radio Selected Unselected)
canvasRadio = foldl1 horiz
              . NEL.toList
              . radioToNEL
              . fmapRadio canvasRadioX canvasRadioO
              . duplicateRadio

listToNEL :: [a] -> Maybe (NEL.NonEmpty a)
listToNEL [] = Nothing
listToNEL (a:as) = Just (a:|as)

makePackage :: (a -> Canvas (ev, a)) -> a -> Package ev a
makePackage f a = Package { _pState = a
                          , _pRender = fmap (\(ev, a') -> (ev, a', makePackage f a')) (f a) }

fmapRadio :: (x -> x') -> (o -> o') -> Radio x o -> Radio x' o'
fmapRadio f g (Chosen x os) = Chosen (f x) (fmap g os)
fmapRadio f g (Unchosen o rs) = Unchosen (g o) (fmapRadio f g rs)

radioToNEL :: Radio a a -> NEL.NonEmpty a
radioToNEL (Chosen x xs) = x :| xs
radioToNEL (Unchosen x xs) = x `NEL.cons` radioToNEL xs


runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = makePackage canvasRadio (Chosen (Selected (L.set (cState.csSelected) True (circleMake "id1")))
                                            [ Unselected (circleMake "id2")
                                            , Unselected (circleMake "id3")
                                            , Unselected (circleMake "id4") ])
{-
  let initialGui = makePackage horizI ((circleMake "id1", circleI)
                                       :| [ (circleMake "id2", circleI) ])
-}
  let loop gui = do
        msg  <- WS.receiveData conn

        let mNextGui = handleMessage (_pRender gui) msg
            nextGui = maybe gui (L.view L._3) mNextGui

        print msg
--        print mNextGui

        WS.sendTextData conn (render (_pRender nextGui))

        loop nextGui

  loop initialGui


main :: IO ()
main = do
  WS.runServer "0.0.0.0" 9998 runServer
