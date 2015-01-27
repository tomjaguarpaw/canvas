{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Doc2 where

import qualified Network.WebSockets as WS
import qualified Data.List.NonEmpty as NEL
import qualified Doc as D
import qualified TextEntry as T
import qualified Select as S
import qualified Circle as C
import qualified Websocket as W
import           Doc (DocF)
import qualified Radio as R
import           Control.Arrow ((***))
import qualified Control.Lens as L
import           Control.Applicative (liftA2)
import qualified Data.Text.Lazy     as DT

data D e c b d = forall s. D (c -> s) (s -> DocF (e, s) d) (s -> b)

pairD :: D e c b d -> D e c' b' d' -> D e (c, c') (b, b') (d, d')
pairD d1 d2 = case d1 of
  D c1 u1 b1 -> case d2 of
    D c2 u2 b2 -> D (c1 *** c2)
                    (\(s1, s2) -> liftA2 (,)
                      (D.fmapResponse (L.over L._2 (\s1n -> (s1n, s2))) (u1 s1))
                      (D.fmapResponse (L.over L._2 (\s2n -> (s1, s2n))) (u2 s2)))
                    (b1 *** b2)

pairE :: D e c b d -> D e' c' b' d' -> D (Either e e') (c, c') (b, b') (d, d')
pairE d d' = pairD (mapEvent Left d) (mapEvent Right d')

mapEvent :: (e -> e') -> D e c b d -> D e' c b d
mapEvent f d1 = case d1 of
  D c u b -> D c (D.fmapResponse (L.over L._1 f) . u) b

mapDoc :: (d -> d') -> D e c b d -> D e c b d'
mapDoc f d1 = case d1 of
  D c u b -> D c (fmap f . u) b

comapCreate :: (c' -> c) -> D e c b d -> D e c' b d
comapCreate f d1 = case d1 of D c u b -> D (c . f) u b

toD :: (a -> DocF (e, a) d) -> D e a a d
toD f = D id f id

handle :: (b -> e -> b -> Maybe c) -> D e c b d -> D e c b d
handle f d1 = case d1 of
  D c1 u1 b1 -> D c1
                  (\so -> D.fmapResponse (\(e, sn) ->
                                           case f (b1 so) e (b1 sn) of
                                             Nothing -> (e, sn)
                                             Just cn -> (e, c1 cn)
                                           ) (u1 so))
                  b1

plainRadio :: W.Widget dxx e x -> W.Widget doo e o
           -> W.Widget (R.Radio dxx doo) e (R.Radio x o)
plainRadio w1 w2 = W.radioW
  W.Component { W.widget  = w1
              , W.handler = just }
  W.Component { W.widget  = w2
              , W.handler = just }
  where just b = W.Response
                  { W.responseEvent = W.event b
                  , W.responseWhole = W.newWhole b }

radio :: D ex cx bx dxx
      -> D eo co bo doo
      -> D (Either (ex, R.RadioX bx bo) (eo, R.RadioO bx bo))
           (R.Radio cx co) (R.Radio bx bo) (R.Radio dxx doo)
radio d1 d2 = case d1 of
  D c1 u1 b1 -> case d2 of
    D c2 u2 b2 -> D (R.fmapRadio c1 c2)
                    ((fmap . D.fmapResponse . L.over L._1)
                            (bimapEither (L.over L._2 (R.fmapRadioX b1 b2))
                                         (L.over L._2 (R.fmapRadioO b1 b2)))
                            (W.radioC u1 u2))
                    (R.fmapRadio b1 b2)

bimapEither :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
bimapEither f g = \case Left  a -> Left  (f a)
                        Right a -> Right (g a)

attach :: D e c b d -> D (e, a) (c, a) b d
attach d1 = case d1 of
  D c1 u1 b1 -> D (L.over L._1 c1)
                  (\(s, a) -> D.fmapResponse (\(e, sn) -> ((e, a), (sn, a))) (u1 s))
                  (b1 . fst)

radioC :: D ()
            (R.Radio C.Selected C.Unselected) (R.Radio C.Selected C.Unselected)
            (R.Radio [D.GUICircle] [D.GUICircle])
radioC = (mapEvent (const ())
          . handle (\_ t _ -> radioClickHandler t))
         (radio (toD C.selectedC) (toD C.unselectedC))

radioClickHandler :: Either a (C.CircleEvent, R.RadioO C.Selected C.Unselected)
                  -> Maybe (R.Radio C.Selected C.Unselected)
radioClickHandler = \case {
  Right (C.MouseClick, xc) ->
     Just (R.choose (C.selectedOfUnselected (R.focusedO xc))
                    C.unselectedOfSelected xc);
  _                        -> Nothing }

radioC' :: D ()
             (R.Radio C.Selected C.Unselected) (R.Radio C.Selected C.Unselected)
             [D.Element]
radioC' = mapDoc (return . D.GUICircles . concat . NEL.toList . R.radioToNEL) radioC

textSelect :: D () (T.TextEntry, S.Select) (T.TextEntry, S.Select) [D.Element]
textSelect = (mapEvent (const ())
              . handle (\_ ev t -> case ev of
                           Left (T.Input i _) ->
                             Just (L.set (L._2.S.sRadio.R.chosen) i t)
                           Right _ -> let newText = L.view (L._2.S.sRadio.R.chosen) t
                                      in Just ((L.set (L._1.T.tText) newText
                                                . L.set (L._1.T.tPosition)
                                                ((fromIntegral . DT.length) newText)) t))
              . mapDoc (uncurry (++)))
             (pairE (toD T.textEntryC) (toD S.selectC))

runD :: (d -> IO D.Message) -> D e c b d -> c -> IO a
runD f d cs = case d of D c u _ -> run f u (c cs)

run :: (d -> IO D.Message) -> (s -> D.DocF (e, s) d) -> s -> IO a
run f fd s = do
  s' <- run' f fd s
  run f fd s'

run' :: (d -> IO D.Message) -> (s -> D.DocF (e, s) d) -> s -> IO s
run' f fd s = do
  let D.Doc u = fd s
      (d, m)  = D.runUS u
  message <- f d
  return $ case m message of Nothing      -> s
                             Just (_, s') -> s'


vert :: D e c b [D.Element]
     -> D e c' b' [D.Element]
     -> D e (c, c') (b, b') [D.Element]
vert d d' = mapDoc (uncurry (++)) (pairD d d')

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = (R.Chosen C.selectedMake
                          [ C.unselectedMake, C.unselectedMake, C.unselectedMake ],
                    (T.textEntryMake "foo", S.selectMake ("foo" NEL.:| ["bar", "baz"])))

      gui = radioC' `vert` textSelect

      handler d = do
        WS.sendTextData conn (D.renderElements' d)
        msg <- WS.receiveData conn
        print msg
        return msg

  runD handler gui initialGui


main :: IO ()
main = WS.runServer "0.0.0.0" 9998 runServer
