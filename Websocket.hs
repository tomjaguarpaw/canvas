{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Websocket where

import qualified Network.WebSockets as WS
import qualified Data.List.NonEmpty as NEL
import           Radio              (RadioX, RadioO, Radio(Chosen),
                                     radioToNEL, extendRadio)
import qualified Radio              as R
import qualified Doc                as D
import qualified Html               as H
import qualified Button             as B
import           Doc                (handleMessage)
import           Circle             (CircleEvent(MouseClick),
                                     Selected, Unselected,
                                     selectedMake, unselectedMake,
                                     selectedOfUnselected, unselectedOfSelected,
                                     selectedC, unselectedC)
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import           Widget             (Widget, Behaviours(..), Response(..),
                                     Component(..), tupleOfResponse, pair)

canvasRadio :: Widget [H.GUICircle] CircleEvent (Radio Selected Unselected)
canvasRadio = D.mapWidgetDoc (concat . NEL.toList . radioToNEL)
  (radioW Component { widget  = selectedC
                    , handler = \b -> Response
                        { responseEvent = event b
                        , responseWhole = newWhole b } }
          Component { widget  = unselectedC
                    , handler = \b -> Response
                        { responseEvent = event b
                        , responseWhole = case event b of
                          MouseClick -> R.choose (selectedOfUnselected (oldComponent b))
                                        unselectedOfSelected
                                        (oldContext b)
                          _          -> newWhole b } })

elementRadio :: Widget [H.Element] CircleEvent (Radio Selected Unselected)
elementRadio = H.elementOfCircles . canvasRadio

radioW :: Component d1 e1 ev' (Radio x o) (RadioX x o) x
       -> Component d2 e2 ev' (Radio x o) (RadioO x o) o
       -> Widget (Radio d1 d2) ev' (Radio x o)
radioW cx co = R.traverseRadio (A.liftA2 (,))
               . extendRadio fx fo
  where behaviourX ev radioXOld radioXNew =
          Behaviours { oldComponent = R.focusedX radioXOld
                     , oldContext   = radioXOld
                     , oldWhole     = R.stampX radioXOld
                     , newComponent = R.focusedX radioXNew
                     , newContext   = radioXNew
                     , newWhole     = R.stampX radioXNew
                     , event        = ev
                     , fromComponent = \x -> R.stampX (R.setFocusedX x radioXOld)
                     , fromContext   = R.stampX
                     , fromWhole     = id }
        behaviourO ev radioOOld radioONew =
          Behaviours { oldComponent = R.focusedO radioOOld
                     , oldContext   = radioOOld
                     , oldWhole     = R.stampO radioOOld
                     , newComponent = R.focusedO radioONew
                     , newContext   = radioONew
                     , newWhole     = R.stampO radioONew
                     , event        = ev
                     , fromComponent = \o -> R.stampO (R.setFocusedO o radioOOld)
                     , fromContext   = R.stampO
                     , fromWhole     = id }

        fx radioXOld = D.fmapResponse (\(ev, xNew) ->
              let radioXNew = R.setFocusedX xNew radioXOld
              in tupleOfResponse (handler cx (behaviourX ev radioXOld radioXNew)))
                                      (widget cx (R.focusedX radioXOld))

        fo radioOOld = D.fmapResponse (\(ev, oNew) ->
              let radioONew = R.setFocusedO oNew radioOOld
              in tupleOfResponse (handler co (behaviourO ev radioOOld radioONew)))
                                      (widget co (R.focusedO radioOOld))

resetter :: Widget [H.Element] () (Radio Selected Unselected, B.Button)
resetter = D.mapWidgetDoc (uncurry (++)) $ pair
  Component { widget  = elementRadio
            , handler = \b -> Response { responseEvent = ()
                                       , responseWhole = newWhole b } }
  Component { widget  = B.buttonC
            , handler = \b -> Response { responseEvent = ()
                                       , responseWhole =
                                      L.over L._1 chooseFirst' (newWhole b) } }
  where chooseFirst' = R.chooseFirst selectedOfUnselected unselectedOfSelected

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = (Chosen selectedMake
                           [ unselectedMake, unselectedMake, unselectedMake ],
                    B.buttonMake "Reset")

  loopGUI conn resetter initialGui

loopGUI :: WS.Connection -> (a -> D.Doc [H.Element] (ev, a)) -> a -> IO b
loopGUI conn canvas gui = do
  let canvas' = (fmap . D.fmapResponse) snd canvas

  msg  <- WS.receiveData conn

  let mNextGui = handleMessage (canvas' gui) msg
      nextGui = maybe gui id mNextGui

  print msg

  WS.sendTextData conn (H.renderElements (canvas' nextGui))

  loopGUI conn canvas nextGui


main :: IO ()
main = do
  WS.runServer "0.0.0.0" 9998 runServer
