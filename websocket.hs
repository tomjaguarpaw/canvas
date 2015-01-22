{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Network.WebSockets as WS
import qualified Data.List.NonEmpty as NEL
import           Radio              (RadioX, RadioO, Radio(Chosen),
                                     radioToNEL, fmapRadio,
                                     duplicateRadio, focusedX, focusedO)
import qualified Radio              as R
import qualified Doc                as D
import qualified Button             as B
import           Doc                (horiz, handleMessage)
import           Circle             (CircleEvent(MouseClick),
                                     Selected, Unselected,
                                     selectedMake, unselectedMake,
                                     selectedOfUnselected, unselectedOfSelected,
                                     selectedC, unselectedC)
import           Data.Monoid        ((<>))
import qualified Data.Text.Lazy     as T

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

elementRadio :: WidgetD [D.Element] CircleEvent (Radio Selected Unselected)
elementRadio = D.elementOfCircles . canvasRadio

type Widget' ev x x' = WidgetD' [D.GUICircle] ev x x'
type Widget  ev x = Widget' ev x x
type Handler ev ev' xz x' xa = (ev, x') -> xz -> (ev', xa)

type WidgetD' d ev x x' = x -> D.Doc d (ev, x')
type WidgetD  d ev x = WidgetD' d ev x x

type Component ev ev' x xz xa = ComponentD [D.GUICircle] ev ev' x xz xa

data ComponentD d ev ev' x xz xa = Component { widget  :: WidgetD d ev x
                                             , handler :: Handler  ev ev' xz x xa }

componentCanvas :: ComponentD d ev ev' x xz xa -> (xz -> x) -> xz -> D.Doc d (ev', xa)
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
     -> WidgetD [D.Element] ev' x'
     -> WidgetD [D.Element] (Either ev ev') (x, x')
vert w w' = vertW Component { widget  = w
                            , handler = \(ev, x) (_, y) -> (Left ev, (x, y)) }
                  Component { widget  = w'
                            , handler = \(ev, y) (x, _) -> (Right ev, (x, y)) }

vertW :: ComponentD [D.Element] e1 ev' x (x, y) (x, y)
      -> ComponentD [D.Element] e2 ev' y (x, y) (x, y)
      -> WidgetD [D.Element] ev' (x, y)
vertW cl cr t = fl t `D.vert` fr t
  where fl = componentCanvas cl fst
        fr = componentCanvas cr snd

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui s = Chosen (selectedMake (t 1))
                     [ unselectedMake (t 2)
                     , unselectedMake (t 3)
                     , unselectedMake (t 4) ]
        where t i = "id" <> T.pack (show (i :: Int)) <> T.pack (show (s :: Int))

  loopGUI conn (elementRadio `vert` B.buttonC)
               (initialGui 1, B.buttonMake "inactive" "idb1")

loopGUI :: WS.Connection -> (a -> D.Doc [D.Element] (ev, a)) -> a -> IO b
loopGUI conn canvas gui = do
  let canvas' = (fmap . fmap) snd canvas

  msg  <- WS.receiveData conn

  let mNextGui = handleMessage (canvas' gui) msg
      nextGui = maybe gui id mNextGui

  print msg

  WS.sendTextData conn (D.renderElements (canvas' nextGui))

  loopGUI conn canvas nextGui


main :: IO ()
main = do
  WS.runServer "0.0.0.0" 9998 runServer
