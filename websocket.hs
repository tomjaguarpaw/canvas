{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Text.Lazy     as T
import qualified Network.WebSockets as WS
import qualified Control.Lens       as L
import qualified Data.List.NonEmpty as NEL
import           Radio              (RadioX, RadioO, Radio(Chosen),
                                     radioToNEL, fmapRadio,
                                     duplicateRadio, focusedX, focusedO)
import qualified Radio              as R
import           Doc                (Canvas(Canvas), GUICircle(GUICircle),
                                     gcName, gcColor, horiz, handleMessage,
                                     render)

data CircleEvent = MouseOver | MouseOut | MouseClick deriving Show
data CircleState = CircleState { _csHovered  :: Bool
                               , _csSelected :: Bool } deriving Show

data Circle = Circle { _cName  :: T.Text
                     , _cState :: CircleState } deriving Show
$(L.makeLenses ''Circle)
$(L.makeLenses ''CircleState)

parseCircleEvent :: T.Text -> Maybe CircleEvent
parseCircleEvent = \case "mouseover" -> Just MouseOver
                         "mouseout"  -> Just MouseOut
                         "click"     -> Just MouseClick
                         _           -> Nothing

circleColor :: CircleState -> T.Text
circleColor c = case (L.view csHovered c, L.view csSelected c)
                of (True, True)   -> "#cc0000"
                   (True, False)  -> "#cccccc"
                   (False, True)  -> "#ff0000"
                   (False, False) -> "#ffffff"

circleMake :: T.Text -> Circle
circleMake n = Circle n (CircleState False False)

circleHandle :: CircleEvent -> Circle -> Circle
circleHandle MouseOver  = L.set  (cState.csHovered)  True
circleHandle MouseOut   = L.set  (cState.csHovered)  False
circleHandle MouseClick = L.over (cState.csSelected) not

guiCircle :: Circle -> GUICircle
guiCircle c = GUICircle { gcName  = _cName c
                        , gcColor = (circleColor . L.view cState) c }

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

data Selected = Selected Circle

data Unselected = Unselected Circle

selectedC :: Selected -> Canvas (CircleEvent, Selected)
selectedC s@(Selected c) = fmap (\ev -> (ev, selectedHandle ev s)) (circle c)

unselectedC :: Unselected -> Canvas (CircleEvent, Unselected)
unselectedC s@(Unselected c) = fmap (\ev -> (ev, unselectedHandle ev s)) (circle c)

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

handlerRadioX :: (ev, x) -> RadioX x o -> (ev, Radio x o)
handlerRadioX (ev, x') rx = (ev, R.stampFocusedX x' rx)

handlerRadioO :: (CircleEvent, Unselected)
              -> RadioO Selected Unselected
              -> (CircleEvent, Radio Selected Unselected)
handlerRadioO (ev, n) b =
  (ev, case ev of
      MouseClick -> R.choose (selectedOfUnselected n) unselectedOfSelected b
      _          -> R.stampFocusedO n b)

canvasRadio :: Widget CircleEvent (Radio Selected Unselected)
canvasRadio = radioW Component { widget  = selectedC
                               , handler = handlerRadioX }
                     Component { widget  = unselectedC
                               , handler = handlerRadioO }

type Widget' ev x x' = x -> Canvas (ev, x')
type Widget  ev x = Widget' ev x x
type Handler ev ev' xz x' xa = (ev, x') -> xz -> (ev', xa)

data Component ev ev' x xz xa = Component { widget  :: Widget ev x
                                          , handler :: Handler ev ev' xz x xa }

componentCanvas :: Component ev ev' x xz xa -> (xz -> x) -> xz -> Canvas (ev', xa)
componentCanvas cg x xz = fmap (\ex' -> handler cg ex' xz) (widget cg (x xz))

radioW :: Component e1 ev' x (RadioX x o) (Radio x o)
       -> Component e2 ev' o (RadioO x o) (Radio x o)
       -> Widget ev' (Radio x o)
radioW cx co = foldl1 horiz
               . NEL.toList
               . radioToNEL
               . fmapRadio fx fo
               . duplicateRadio
  where fx = componentCanvas cx focusedX
        fo = componentCanvas co focusedO

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = Chosen (Selected (L.set (cState.csSelected) True (circleMake "id1")))
                   [ Unselected (circleMake "id2")
                   , Unselected (circleMake "id3")
                   , Unselected (circleMake "id4") ]
  loopGUI conn canvasRadio initialGui

loopGUI :: WS.Connection -> (a -> Canvas (ev, a)) -> a -> IO b
loopGUI conn canvas gui = do
  let canvas' = (fmap . fmap) snd canvas

  msg  <- WS.receiveData conn

  let mNextGui = handleMessage (canvas' gui) msg
      nextGui = maybe gui id mNextGui

  print msg

  WS.sendTextData conn (render (canvas' nextGui))

  loopGUI conn canvas nextGui


main :: IO ()
main = do
  WS.runServer "0.0.0.0" 9998 runServer
