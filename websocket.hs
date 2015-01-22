{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Network.WebSockets as WS
import qualified Data.List.NonEmpty as NEL
import           Radio              (RadioX, RadioO, Radio(Chosen),
                                     radioToNEL, fmapRadio,
                                     duplicateRadio, focusedX, focusedO)
import qualified Radio              as R
import qualified Doc                as D
import           Doc                (Canvas, horiz, handleMessage, render)
import           Circle             (CircleEvent(MouseClick),
                                     Selected, Unselected,
                                     selectedMake, unselectedMake,
                                     selectedOfUnselected, unselectedOfSelected,
                                     selectedC, unselectedC)

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

type Widget' ev x x' = WidgetD' [D.GUICircle] ev x x'
type Widget  ev x = Widget' ev x x
type Handler ev ev' xz x' xa = (ev, x') -> xz -> (ev', xa)

type WidgetD' d ev x x' = x -> D.Doc d (ev, x')
type WidgetD  d ev x = WidgetD' d ev x x

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

vert :: WidgetD [D.Element] ev x
     -> WidgetD [D.Element] ev x'
     -> WidgetD [D.Element] ev (x, x')
vert w w' (x, x') = fmap (\(ev, y) -> (ev, (y, x'))) (w x)
                    `D.vert`
                    fmap (\(ev, y') -> (ev, (x, y'))) (w' x')

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = Chosen (selectedMake "id1")
                   [ unselectedMake "id2"
                   , unselectedMake "id3"
                   , unselectedMake "id4" ]
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
