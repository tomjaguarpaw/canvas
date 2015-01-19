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
import           Data.Maybe         (fromMaybe)
import           Radio              (RadioX(At), RadioO(Before, After),
                                     Radio(Chosen),
                                     stampO, stampX, radioToNEL, fmapRadio,
                                     duplicateRadio)

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

canvasRadioX :: RadioX Selected Unselected
             -> Canvas (CircleEvent, Radio Selected Unselected)
canvasRadioX (At ls x rs) = fmap (\(ev, x') -> (ev, stampX (At ls x' rs)))
                                 (selectedC x)

unselect :: Radio Selected Unselected -> [Unselected]
unselect = NEL.toList . radioToNEL . fmapRadio unselectedOfSelected id

canvasRadioO :: RadioO Selected Unselected
             -> Canvas (CircleEvent, Radio Selected Unselected)
canvasRadioO = \case
  Before rs o os -> fmap (\(ev, n) -> (ev, case ev of
                                            MouseClick ->
                                              stampX (At
                                                (unselect rs)
                                                (selectedOfUnselected n)
                                                os)
                                            _ ->
                                              stampO (Before rs n os)
                                        ))
                           (unselectedC o)
  After os o rs -> fmap (\(ev, n) -> (ev, case ev of
                                           MouseClick ->
                                             stampX (At
                                               os
                                               (selectedOfUnselected n)
                                               (unselect rs))
                                           _ ->
                                             stampO (After os n rs)
                                       ))
                           (unselectedC o)


canvasRadio :: Radio Selected Unselected -> Canvas (Radio Selected Unselected)
canvasRadio = fmap snd
              . foldl1 horiz
              . NEL.toList
              . radioToNEL
              . fmapRadio canvasRadioX canvasRadioO
              . duplicateRadio

data Loop f = Loop { runLoop :: f (Loop f) }

runF :: Functor f => (a -> f a) -> a -> Loop f
runF f a = Loop (fmap (runF f) (f a))

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = runF canvasRadio (Chosen (Selected (L.set (cState.csSelected) True (circleMake "id1")))
                                          [ Unselected (circleMake "id2")
                                          , Unselected (circleMake "id3")
                                          , Unselected (circleMake "id4") ])
{-
  let initialGui = makePackage horizI ((circleMake "id1", circleI)
                                       :| [ (circleMake "id2", circleI) ])
-}
  let loop gui = do
        msg  <- WS.receiveData conn

        let mNextGui = handleMessage (runLoop gui) msg
            nextGui = fromMaybe gui mNextGui

        print msg
--        print mNextGui

        WS.sendTextData conn (render (runLoop nextGui))

        loop nextGui

  loop initialGui


main :: IO ()
main = do
  WS.runServer "0.0.0.0" 9998 runServer
