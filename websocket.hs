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

canvasRadio :: Widget CircleEvent (Radio Selected Unselected)
canvasRadio = ((fmap.fmap) (concat . NEL.toList) . (fmap.fmap) radioToNEL)
              (radioW'' Component2 { widget2  = selectedC
                                   , handler2 = handler2OfHandler handlerRadioX }
                        Component2 { widget2  = unselectedC
                                   , handler2 = handler2OfHandler handlerRadioO })

elementRadio :: WidgetD [D.Element] CircleEvent (Radio Selected Unselected)
elementRadio = D.elementOfCircles . canvasRadio

type Widget' ev x x' = WidgetD' [D.GUICircle] ev x x'
type Widget  ev x = Widget' ev x x
type Handler ev ev' xz x' xa = (ev, x') -> xz -> (ev', xa)

type WidgetD' d ev x x' = x -> D.Doc d (ev, x')
type WidgetD  d ev x = WidgetD' d ev x x

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

type Component ev ev' x xz xa = ComponentD [D.GUICircle] ev ev' x xz xa

data ComponentD d ev ev' x xz xa = Component { widget  :: WidgetD d ev x
                                             , handler :: Handler  ev ev' xz x xa }

data Component2 d ev ev' xa xc x = Component2 { widget2  :: WidgetD d ev x
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

component2OfComponentD :: ComponentD d ev ev' x xc xa -> Component2 d ev ev' xa xc x
component2OfComponentD cd = Component2 { widget2  = widget cd
                                       , handler2 = handler2OfHandler (handler cd)
                                       }

radioW'' :: Component2 d1 e1 ev' (Radio x o) (RadioX x o) x
         -> Component2 d2 e2 ev' (Radio x o) (RadioO x o) o
         -> WidgetD (Radio d1 d2) ev' (Radio x o)
radioW'' cx co = R.traverseRadio (A.liftA2 (,))
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

radioW :: Component e1 ev' x (RadioX x o) (Radio x o)
       -> Component e2 ev' o (RadioO x o) (Radio x o)
       -> Widget ev' (Radio x o)
radioW cx co = (fmap.fmap) (concat . NEL.toList) (radioW' cx co)

radioW' :: ComponentD d e1 ev' x (RadioX x o) (Radio x o)
        -> ComponentD d e2 ev' o (RadioO x o) (Radio x o)
        -> WidgetD (NEL.NonEmpty d) ev' (Radio x o)
radioW' cx co = fmap radioToNEL
                . radioW'' (component2OfComponentD cx) (component2OfComponentD co)

vert :: WidgetD [D.Element] ev x
     -> WidgetD [D.Element] ev' x'
     -> WidgetD [D.Element] (Either ev ev') (x, x')
vert w w' = vertW Component { widget  = w
                            , handler = \(ev, x) (_, y) -> (Left ev, (x, y)) }
                  Component { widget  = w'
                            , handler = \(ev, y) (x, _) -> (Right ev, (x, y)) }

resetter :: WidgetD [D.Element] () (Radio Selected Unselected, B.Button)
resetter = vertW Component { widget  = elementRadio
                           , handler = \(_, x) (_, y) -> ((), (x, y)) }
                 Component { widget  = B.buttonC
                           , handler = \(_, y) (x, _) -> ((), (chooseFirst' x, y)) }
  where chooseFirst' = R.chooseFirst selectedOfUnselected unselectedOfSelected


vertW :: ComponentD [D.Element] e1 ev' x (x, y) (x, y)
      -> ComponentD [D.Element] e2 ev' y (x, y) (x, y)
      -> WidgetD [D.Element] ev' (x, y)
vertW cl cr t = fl t `D.vert` fr t
  where fl = componentCanvas cl fst
        fr = componentCanvas cr snd

textSelect :: WidgetD [D.Element] () (T.TextEntry, S.Select)
textSelect = vertW Component { widget  = T.textEntryC
                             , handler = \(T.Input i _, t) (_, s) ->
                                             ((), (t, L.set (S.sRadio.R.chosen) i s)) }
                   Component { widget  = S.selectC
                             , handler = \(_, y) (x, _) ->
                             let newText = L.view (S.sRadio.R.chosen) y
                             in ((), ((L.set T.tText newText
                                       . L.set T.tPosition ((fromIntegral . DT.length) newText)) x, y)) }

textSelect' :: WidgetD [D.Element] () (T.TextEntry, S.Select)
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
