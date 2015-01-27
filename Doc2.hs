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

toD :: (a -> DocF (e, a) d) -> D e a a d
toD f = D id f id

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

radio :: D e c1 b1 d1
      -> D e c2 b2 d2
      -> D e (R.Radio c1 c2) (R.Radio b1 b2) (R.Radio d1 d2)
radio d1 d2 = case d1 of
  D c1 u1 b1 -> case d2 of
    D c2 u2 b2 -> D (R.fmapRadio c1 c2)
                    (plainRadio u1 u2)
                    (R.fmapRadio b1 b2)
