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
import qualified Data.Text.Lazy     as DT
import qualified Control.Applicative as A

canvasRadio :: Widget [D.GUICircle] CircleEvent (Radio Selected Unselected)
canvasRadio = (D.mapWidgetDoc (concat . NEL.toList . radioToNEL))
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

type Widget' d ev x x' = x -> D.Doc d (ev, x')
type Widget  d ev x = Widget' d ev x x

data Behaviours ev xa xc x = Behaviours { oldComponent :: x
                                        , oldContext   :: xc
                                        , oldWhole     :: xa
                                        , newComponent :: x
                                        , newContext   :: xc
                                        , newWhole     :: xa
                                        , event        :: ev
                                        , fromComponent :: x  -> xa
                                        , fromContext   :: xc -> xa
                                        , fromWhole     :: xa -> xa }

data Response ev xa = Response { responseWhole :: xa
                               , responseEvent :: ev }

data Component d ev ev' xa xc x = Component { widget  :: Widget d ev x
                                            , handler :: Behaviours ev xa xc x
                                                      -> Response ev' xa }

tupleOfResponse :: Response ev xa -> (ev, xa)
tupleOfResponse r = (responseEvent r, responseWhole r)

vertW' :: Component d1 e1 ev' (x, y) (x, y) x
       -> Component d2 e2 ev' (x, y) (x, y) y
       -> Widget (d1, d2) ev' (x, y)
vertW' cx cy = supertraverse fx fy
  where behaviourX ev told tnew = Behaviours {
            oldComponent = fst told
          , oldContext   = told
          , oldWhole     = told
          , newComponent = fst tnew
          , newContext   = tnew
          , newWhole     = tnew
          , event        = ev
          , fromComponent = \x -> L.set L._1 x told
          , fromContext   = id
          , fromWhole     = id }
        behaviourY ev told tnew = Behaviours {
            oldComponent = snd told
          , oldContext   = told
          , oldWhole     = told
          , newComponent = snd tnew
          , newContext   = tnew
          , newWhole     = tnew
          , event        = ev
          , fromComponent = \y -> L.set L._2 y told
          , fromContext   = id
          , fromWhole     = id }

        fx told = D.fmapResponse (\(ev, xNew) ->
          let tnew = L.set L._1 xNew told
          in tupleOfResponse (handler cx (behaviourX ev told tnew)))
                  (widget cx (fst told))

        fy told = D.fmapResponse (\(ev, yNew) ->
          let tnew = L.set L._2 yNew told
          in tupleOfResponse (handler cy (behaviourY ev told tnew)))
                  (widget cy (snd told))

        supertraverse gx gy (x, y) = A.liftA2 (,) (gx (x, y)) (gy (x, y))


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


textSelect :: Widget [D.Element] () (T.TextEntry, S.Select)
textSelect = D.mapWidgetDoc (uncurry (++)) $ vertW'
  Component { widget  = T.textEntryC
            , handler = \b ->
                Response { responseEvent = ()
                         , responseWhole =
                              let T.Input i _ = event b
                              in L.set (L._2.S.sRadio.R.chosen) i (newWhole b) } }
  Component { widget  = S.selectC
            , handler = \b ->
                Response { responseEvent = ()
                         , responseWhole =
                              let newW = newWhole b
                                  newText = L.view (L._2.S.sRadio.R.chosen) newW
                              in (L.set (L._1.T.tText) newText
                                  . L.set (L._1.T.tPosition)
                                  ((fromIntegral . DT.length) newText))
                                 newW } }

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = Chosen selectedMake
                          [ unselectedMake, unselectedMake, unselectedMake ]

  loopGUI conn (resetter `vert` textSelect)
               (((initialGui, B.buttonMake "Reset"),
                 (T.textEntryMake "foo", S.selectMake ("foo" NEL.:| ["bar", "baz"]))))

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
