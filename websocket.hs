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
import           Control.Applicative ((<$>), (<*>))
import qualified Control.Applicative as A

handlerRadioX :: (ev, x) -> RadioX x o -> (ev, Radio x o)
handlerRadioX (ev, x') rx = (ev, R.stampFocusedX x' rx)

handlerRadioO :: (CircleEvent, Unselected)
              -> RadioO Selected Unselected
              -> (CircleEvent, Radio Selected Unselected)
handlerRadioO (ev, n) b =
  (ev, case ev of
      MouseClick -> R.choose (selectedOfUnselected n) unselectedOfSelected b
      _          -> R.stampFocusedO n b)

canvasRadio :: Widget [D.GUICircle] CircleEvent (Radio Selected Unselected)
canvasRadio = ((fmap.fmap) (concat . NEL.toList . radioToNEL))
              (radioW Component { widget2  = selectedC
                                , handler2 = handler2OfHandler handlerRadioX }
                      Component { widget2  = unselectedC
                                , handler2 = handler2OfHandler handlerRadioO })

elementRadio :: Widget [D.Element] CircleEvent (Radio Selected Unselected)
elementRadio = D.elementOfCircles . canvasRadio

type Handler ev ev' xz x' xa = (ev, x') -> xz -> (ev', xa)

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

data ComponentD d ev ev' x xz xa = ComponentD { widget  :: Widget d ev x
                                              , handler :: Handler  ev ev' xz x xa }

data Component d ev ev' xa xc x = Component { widget2  :: Widget d ev x
                                            , handler2 :: Behaviours ev xa xc x
                                                       -> Outputs xa xc x
                                                       -> Response ev' xa }

tupleOfResponse :: Response ev xa -> (ev, xa)
tupleOfResponse r = (responseEvent r, responseWidget r)

handler2OfHandler :: Handler t ev xc x xa1
                  -> Behaviours t xa xc x
                  -> Outputs xa1 xc1 x1
                  -> Response ev xa1
handler2OfHandler h b o = let (ev', xa) = h (event b, newComponent b) (oldContext b)
                          in Response { responseWidget = fromWidget o xa
                                      , responseEvent  = ev' }

vertW' :: Component d1 e1 ev' (x, y) (x, y) x
       -> Component d2 e2 ev' (x, y) (x, y) y
       -> Widget (d1, d2) ev' (x, y)
vertW' cx cy = supertraverse fx fy
  where outputX (x, y) = Outputs { fromComponent = \x' -> (x', y)
                                 , fromContext   = id
                                 , fromWidget    = id }
        outputY (x, y) = Outputs { fromComponent = \y' -> (x, y')
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


componentCanvas :: ComponentD d ev ev' x xz xa -> (xz -> x) -> xz -> D.Doc d (ev', xa)
componentCanvas cg x xz = D.fmapResponse (\ex' -> handler cg ex' xz) (widget cg (x xz))

vert :: Widget [D.Element] ev x
     -> Widget [D.Element] ev' x'
     -> Widget [D.Element] (Either ev ev') (x, x')
vert w w' = vertW ComponentD { widget  = w
                            , handler = \(ev, x) (_, y) -> (Left ev, (x, y)) }
                  ComponentD { widget  = w'
                            , handler = \(ev, y) (x, _) -> (Right ev, (x, y)) }

resetter :: Widget [D.Element] () (Radio Selected Unselected, B.Button)
resetter = vertW ComponentD { widget  = elementRadio
                           , handler = \(_, x) (_, y) -> ((), (x, y)) }
                 ComponentD { widget  = B.buttonC
                           , handler = \(_, y) (x, _) -> ((), (chooseFirst' x, y)) }
  where chooseFirst' = R.chooseFirst selectedOfUnselected unselectedOfSelected


vertW :: ComponentD [D.Element] e1 ev' x (x, y) (x, y)
      -> ComponentD [D.Element] e2 ev' y (x, y) (x, y)
      -> Widget [D.Element] ev' (x, y)
vertW cl cr t = fl t `D.vert` fr t
  where fl = componentCanvas cl fst
        fr = componentCanvas cr snd

textSelect :: Widget [D.Element] () (T.TextEntry, S.Select)
textSelect = vertW ComponentD { widget  = T.textEntryC
                             , handler = \(T.Input i _, t) (_, s) ->
                                             ((), (t, L.set (S.sRadio.R.chosen) i s)) }
                   ComponentD { widget  = S.selectC
                             , handler = \(_, y) (x, _) ->
                             let newText = L.view (S.sRadio.R.chosen) y
                             in ((), ((L.set T.tText newText
                                       . L.set T.tPosition ((fromIntegral . DT.length) newText)) x, y)) }

textSelect' :: Widget [D.Element] () (T.TextEntry, S.Select)
textSelect' o = (++) <$> D.fmapResponse (\(_, newte) -> ((), (newte, textSelected newte (old se))))
                                        (T.textEntryC (old te))


                     <*> D.fmapResponse (\(_, newse) -> ((),
                           let newText = L.view (S.sRadio.R.chosen) newse
                           in (textUpdated newText (old te), newse)))
                                        (S.selectC (old se))
  where old = ($ o)
        te  = fst
        se  = snd

        textUpdated new = L.set T.tText new
                          . L.set T.tPosition ((fromIntegral . DT.length) new)

        textSelected new = L.set (S.sRadio.R.chosen) (L.view T.tText new)



runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = Chosen selectedMake
                          [ unselectedMake, unselectedMake, unselectedMake ]

  loopGUI conn (resetter `vert` textSelect')
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
