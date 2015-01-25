{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Network.WebSockets as WS
import qualified Data.List.NonEmpty as NEL
import           Radio              (RadioX, RadioO, Radio(Chosen),
                                     radioToNEL, fmapRadio,
                                     duplicateRadio)
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
canvasRadio = ((fmap.fmap) (concat . NEL.toList . radioToNEL))
              (radioW Component { widget2  = selectedC
                                , handler2 = \b _ -> Response
                                     { responseEvent  = event b
                                     , responseWidget = newWidget b } }
                      Component { widget2  = unselectedC
                                , handler2 = \b _ -> Response
                                     { responseEvent  = event b
                                     , responseWidget = case event b of
                                         MouseClick -> R.choose (selectedOfUnselected (oldComponent b))
                                                                unselectedOfSelected
                                                                (oldContext b)
                                         _          -> newWidget b } })

elementRadio :: Widget [D.Element] CircleEvent (Radio Selected Unselected)
elementRadio = D.elementOfCircles . canvasRadio

type Widget' d ev x x' = x -> D.Doc d (ev, x')
type Widget  d ev x = Widget' d ev x x

data Behaviours ev xa xc x = Behaviours { oldComponent :: x
                                        , oldContext   :: xc
                                        , oldWidget    :: xa
                                        , newComponent :: x
                                        , newContext   :: xc
                                        , newWidget    :: xa
                                        , event        :: ev }

data Response ev xa = Response { responseWidget :: xa
                               , responseEvent  :: ev }

data Outputs xa xc x = Outputs { fromComponent :: x  -> xa
                               , fromContext   :: xc -> xa
                               , fromWidget    :: xa -> xa }

data Package ev xa xc x = Package { behaviours :: Behaviours ev xa xc x
                                  , outputs    :: Outputs xa xc x }

data Component d ev ev' xa xc x = Component { widget2  :: Widget d ev x
                                            , handler2 :: Behaviours ev xa xc x
                                                       -> Outputs xa xc x
                                                       -> Response ev' xa }

tupleOfResponse :: Response ev xa -> (ev, xa)
tupleOfResponse r = (responseEvent r, responseWidget r)

vertW' :: Component d1 e1 ev' (x, y) (x, y) x
       -> Component d2 e2 ev' (x, y) (x, y) y
       -> Widget (d1, d2) ev' (x, y)
vertW' cx cy = supertraverse fx fy
  where outputX t = Outputs { fromComponent = \x -> L.set L._1 x t
                            , fromContext   = id
                            , fromWidget    = id }
        outputY t = Outputs { fromComponent = \y -> L.set L._2 y t
                            , fromContext   = id
                            , fromWidget    = id }
        behaviourX ev told tnew = Behaviours {
            oldComponent = fst told
          , oldContext   = told
          , oldWidget    = told
          , newComponent = fst tnew
          , newContext   = tnew
          , newWidget    = tnew
          , event        = ev }
        behaviourY ev told tnew = Behaviours {
            oldComponent = snd told
          , oldContext   = told
          , oldWidget    = told
          , newComponent = snd tnew
          , newContext   = tnew
          , newWidget    = tnew
          , event        = ev }

        fx told = D.fmapResponse (\(ev, xNew) ->
          let tnew = L.set L._1 xNew told
          in tupleOfResponse (handler2 cx (behaviourX ev told tnew)
                                       (outputX tnew)))
                  (widget2 cx (fst told))

        fy told = D.fmapResponse (\(ev, yNew) ->
          let tnew = L.set L._2 yNew told
          in tupleOfResponse (handler2 cy (behaviourY ev told tnew)
                                       (outputY tnew)))
                  (widget2 cy (snd told))

        supertraverse gx gy (x, y) = A.liftA2 (,) (gx (x, y)) (gy (x, y))


radioW :: Component d1 e1 ev' (Radio x o) (RadioX x o) x
       -> Component d2 e2 ev' (Radio x o) (RadioO x o) o
       -> Widget (Radio d1 d2) ev' (Radio x o)
radioW cx co = R.traverseRadio (A.liftA2 (,))
               . fmapRadio fx fo
               . duplicateRadio
  where outputX radioX = Outputs { fromComponent = \x -> R.stampX (R.setFocusedX x radioX)
                                 , fromContext   = R.stampX
                                 , fromWidget    = id }
        outputO radioO = Outputs { fromComponent = \o -> R.stampO (R.setFocusedO o radioO)
                                 , fromContext   = R.stampO
                                 , fromWidget    = id }
        behaviourX ev radioXOld radioXNew = Behaviours { oldComponent = R.focusedX radioXOld
                                                       , oldContext   = radioXOld
                                                       , oldWidget    = R.stampX radioXOld
                                                       , newComponent = R.focusedX radioXNew
                                                       , newContext   = radioXNew
                                                       , newWidget    = R.stampX radioXNew
                                                       , event        = ev }
        behaviourO ev radioOOld radioONew = Behaviours { oldComponent = R.focusedO radioOOld
                                                       , oldContext   = radioOOld
                                                       , oldWidget    = R.stampO radioOOld
                                                       , newComponent = R.focusedO radioONew
                                                       , newContext   = radioONew
                                                       , newWidget    = R.stampO radioONew
                                                       , event        = ev }

        fx radioXOld = D.fmapResponse (\(ev, xNew) ->
              let radioXNew = R.setFocusedX xNew radioXOld
              in tupleOfResponse (handler2 cx (behaviourX ev radioXOld radioXNew)
                             (outputX radioXNew)))
                                      (widget2 cx (R.focusedX radioXOld))

        fo radioOOld = D.fmapResponse (\(ev, oNew) ->
              let radioONew = R.setFocusedO oNew radioOOld
              in tupleOfResponse (handler2 co (behaviourO ev radioOOld radioONew)
                             (outputO radioONew)))
                                      (widget2 co (R.focusedO radioOOld))


vert :: Widget [D.Element] ev x
     -> Widget [D.Element] ev' x'
     -> Widget [D.Element] (Either ev ev') (x, x')
vert w w' = (fmap.fmap) (uncurry (++)) $
            vertW' Component { widget2 = w
                             , handler2 = \b _ -> Response { responseEvent  = Left (event b)
                                                           , responseWidget = newWidget b } }
                   Component { widget2 = w'
                             , handler2 = \b _ -> Response { responseEvent  = Right (event b)
                                                           , responseWidget = newWidget b } }



resetter :: Widget [D.Element] () (Radio Selected Unselected, B.Button)
resetter = (fmap.fmap) (uncurry (++)) $
           vertW' Component { widget2  = elementRadio
                            , handler2 = \b _ -> Response { responseEvent  = ()
                                                          , responseWidget = newWidget b } }
                  Component { widget2  = B.buttonC
                            , handler2 = \b _ -> Response { responseEvent = ()
                                                          , responseWidget =
                                                               L.over L._1 chooseFirst' (newWidget b) } }
  where chooseFirst' = R.chooseFirst selectedOfUnselected unselectedOfSelected


textSelect :: Widget [D.Element] () (T.TextEntry, S.Select)
textSelect = (fmap.fmap) (uncurry (++)) $
             vertW' Component { widget2  = T.textEntryC
                              , handler2 = \b _ -> Response { responseEvent  = ()
                                                            , responseWidget =
                                                                 let T.Input i _ = event b
                                                                 in L.set (L._2.S.sRadio.R.chosen) i (newWidget b) } }
                   Component { widget2  = S.selectC
                             , handler2 = \b _ -> Response { responseEvent  = ()
                                                           , responseWidget =
                                                                let newW = newWidget b
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
