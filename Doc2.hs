{-# LANGUAGE ExistentialQuantification #-}

module Doc2 where

import qualified Doc as D
import qualified Websocket as W
import           Doc (DocF)
import qualified Radio as R
import           Control.Arrow ((***))
import qualified Control.Lens as L
import           Control.Applicative (liftA2)

data D e c b d = forall r. D (c -> r) (r -> DocF (e, r) d) (r -> b)

pairD :: D e c b d -> D e c' b' d' -> D e (c, c') (b, b') (d, d')
pairD d1 d2 = case d1 of
  D c1 u1 b1 -> case d2 of
    D c2 u2 b2 -> D (c1 *** c2)
                    (\(r1, r2) -> liftA2 (,)
                      (D.fmapResponse (L.over L._2 (\r1n -> (r1n, r2))) (u1 r1))
                      (D.fmapResponse (L.over L._2 (\r2n -> (r1, r2n))) (u2 r2)))
                    (b1 *** b2)

mapEvent :: (e -> e') -> D e c b d -> D e' c b d
mapEvent f d1 = case d1 of
  D c u b -> D c (D.fmapResponse (L.over L._1 f) . u) b

comapCreate :: (c' -> c) -> D e c b d -> D e c' b d
comapCreate f d1 = case d1 of D c u b -> D (c . f) u b

toD :: (a -> DocF (e, a) d) -> D e a a d
toD f = D id f id

handle :: (b -> e -> b -> Maybe c) -> D e c b d -> D e c b d
handle f d1 = case d1 of
  D c1 u1 b1 -> D c1
                  (\ro -> D.fmapResponse (\(e, rn) ->
                                           case f (b1 ro) e (b1 rn) of
                                             Nothing -> (e, rn)
                                             Just cn -> (e, c1 cn)
                                           ) (u1 ro))
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
                    ((fmap.D.fmapResponse. (L.over L._1))
                            (either (\(e, c) -> Left (e, R.fmapRadioX b1 b2 c))
                                    (\(e, c) -> Right(e, R.fmapRadioO b1 b2 c)))
                            (W.radioC u1 u2))
                    (R.fmapRadio b1 b2)

attach :: D e c b d -> D (e, a) (c, a) b d
attach d1 = case d1 of
  D c1 u1 b1 -> D (L.over L._1 c1)
                  (\(r, a) -> D.fmapResponse (\(e, rn) -> ((e, a), (rn, a))) (u1 r))
                  (b1 . fst)
