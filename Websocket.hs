{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Websocket where

import qualified Network.WebSockets as WS
import qualified Data.List.NonEmpty as NEL
import           Radio              (RadioX, RadioO, Radio(Chosen),
                                     radioToNEL, extendRadio)
import qualified Radio              as R
import qualified Doc                as D
import qualified Button             as B
import qualified TextEntry          as T
import qualified Select             as S
import           Doc                (handleMessage)
import           Circle             (CircleEvent(MouseClick),
                                     Selected, Unselected,
                                     selectedMake, unselectedMake,
                                     selectedOfUnselected, unselectedOfSelected,
                                     selectedC, unselectedC)
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import           Widget             (Widget, Behaviours(..), Response(..),
                                     Component(..), tupleOfResponse, vertW')
import qualified TextSelect         as TS
import qualified Filter             as F

canvasRadio :: Widget [D.GUICircle] CircleEvent (Radio Selected Unselected)
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

elementRadio :: Widget [D.Element] CircleEvent (Radio Selected Unselected)
elementRadio = D.elementOfCircles . canvasRadio

radioC :: Widget dxx evx x -> Widget doo evo o
       -> Widget (Radio dxx doo) (Either (evx, RadioX x o) (evo, RadioO x o))
                 (Radio x o)
radioC wx wo = radioW
  Component { widget  = wx
            , handler = \b -> Response
                { responseEvent = Left (event b, newContext b)
                , responseWhole = newWhole b } }
  Component { widget  = wo
            , handler = \b -> Response
                { responseEvent = Right (event b, newContext b)
                , responseWhole = newWhole b } }

radioE :: Widget dxx evx x -> Widget doo evo o
          -> Widget (Radio dxx doo)
                    (Either
                       (Behaviours evx (Radio x o) (RadioX x o) x)
                       (Behaviours evo (Radio x o) (RadioO x o) o))
                    (Radio x o)
radioE wx wo = radioW
  Component { widget  = wx
            , handler = \b -> Response
                { responseEvent = Left b
                , responseWhole = newWhole b } }
  Component { widget  = wo
            , handler = \b -> Response
                { responseEvent = Right b
                , responseWhole = newWhole b } }


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

radioA :: Widget d1 e1 x -> Widget d2 e2 o
       -> Radio (x, a1) (o, a2)
       -> D.DocF (Either (e1, a1) (e2, a2), Radio (x, a1) (o, a2)) (Radio d1 d2)
radioA wx wo = R.traverseRadio (A.liftA2 (,))
               . extendRadio fx fo
  where fx radioXAOld = D.fmapResponse (\(ev, xNew@(_, a)) ->
              let radioXNew =   R.setFocusedX xNew radioXAOld
              in (Left (ev, a), R.stampX radioXNew))
                        (let (xOld, a) = R.focusedX radioXAOld
                         in D.fmapResponse ((L.over L._2) (\xNew -> (xNew, a))) (wx xOld))
        fo radioOAOld = D.fmapResponse (\(ev, oNew@(_, a)) ->
              let radioONew = R.setFocusedO oNew radioOAOld
              in (Right (ev, a), R.stampO radioONew))
                        (let (oOld, a) = R.focusedO radioOAOld
                         in D.fmapResponse ((L.over L._2) (\xNew -> (xNew, a))) (wo oOld))

vert :: Widget [D.Element] ev x
     -> Widget [D.Element] ev' x'
     -> Widget [D.Element] (Either ev ev') (x, x')
vert w w' = D.mapWidgetDoc (uncurry (++)) $ vertW'
  Component { widget = w
            , handler = \b -> Response { responseEvent = Left (event b)
                                       , responseWhole = newWhole b } }
  Component { widget = w'
            , handler = \b -> Response { responseEvent = Right (event b)
                                       , responseWhole = newWhole b } }

resetter :: Widget [D.Element] () (Radio Selected Unselected, B.Button)
resetter = D.mapWidgetDoc (uncurry (++)) $ vertW'
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

  let initialGui = Chosen selectedMake
                          [ unselectedMake, unselectedMake, unselectedMake ]

  loopGUI conn ((resetter `vert` TS.textSelect) `vert` F.filterB)
               ((((initialGui, B.buttonMake "Reset"),
                 (T.textEntryMake "foo", S.selectMake ("foo" NEL.:| ["bar", "baz"])))),
                F.filterMake)

loopGUI :: WS.Connection -> (a -> D.Doc [D.Element] (ev, a)) -> a -> IO b
loopGUI conn canvas gui = do
  let canvas' = (fmap . D.fmapResponse) snd canvas

  msg  <- WS.receiveData conn

  let mNextGui = handleMessage (canvas' gui) msg
      nextGui = maybe gui id mNextGui

  print msg

  WS.sendTextData conn (D.renderElements (canvas' nextGui))

  loopGUI conn canvas nextGui


main :: IO ()
main = do
  WS.runServer "0.0.0.0" 9998 runServer
