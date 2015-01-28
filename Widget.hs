module Widget where

import qualified Doc                as D
import qualified Control.Lens       as L
import qualified Control.Applicative as A

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

pair :: Widget d1 ev1 x1
     -> Widget d2 ev2 x2
     -> Widget (d1, d2) (Either ev1 ev2) (x1, x2)
pair w1 w2 = vertW'
  Component { widget = w1
            , handler = \b -> Response { responseEvent = Left (event b)
                                       , responseWhole = newWhole b } }
  Component { widget = w2
            , handler = \b -> Response { responseEvent = Right (event b)
                                       , responseWhole = newWhole b } }
