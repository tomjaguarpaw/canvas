{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Doc where

import qualified Data.Text.Lazy     as T
import           Data.Monoid        ((<>))
import qualified Text.Blaze         as B
import           Text.Blaze.Html5   ((!))
import qualified Text.Blaze.Svg11   as S
import qualified Text.Blaze.Svg11.Attributes as AS
import           Text.Blaze.Html.Renderer.Text (renderHtml)

type Message = T.Text

data Doc d a = Doc d (Message -> Maybe a)

instance Functor (Doc d) where
  fmap f (Doc cs h) = Doc cs ((fmap . fmap) f h)

type Canvas a = Doc [GUICircle] a

data GUICircle = GUICircle { gcName  :: T.Text
                           , gcColor :: T.Text } deriving Show

nullCanvas :: Canvas a
nullCanvas = Doc [] (const Nothing)

horiz :: Canvas a -> Canvas a -> Canvas a
horiz (Doc xs xh) (Doc ys yh) = Doc (xs ++ ys)
                                    (\message -> case xh message of
                                        r@(Just _) -> r
                                        Nothing -> case yh message of
                                          s@(Just _) -> s
                                          Nothing -> Nothing)
handleMessage :: Canvas a -> Message -> Maybe a
handleMessage (Doc _ h) = h

render :: Canvas a -> T.Text
render (Doc cs _) = renderHtml (documentSvg h w (sequence_ (package cs [0..])))

  where package = zipWith (\c i -> circleSvg (50 + i * 100) 50 (B.toValue (gcColor c)) (B.toValue (gcName c)))
        w = 100 * length cs
        h = 100

documentSvg :: Int -> Int -> S.Svg -> S.Svg
documentSvg h w = S.svg ! AS.width (B.toValue w)
                        ! AS.height (B.toValue h)

circleSvg :: Int -> Int -> S.AttributeValue -> S.AttributeValue -> S.Svg
circleSvg cx cy color name =
  S.circle ! AS.cx (B.toValue cx)
           ! AS.cy (B.toValue cy)
           ! AS.r "40"
           ! AS.stroke "black"
           ! AS.strokeWidth "4"
           ! AS.fill color
           ! AS.onmouseover ("mouseover('" <> name <> "')")
           ! AS.onmouseout  ("mouseout('" <> name <> "')")
           ! AS.onclick     ("click('" <> name <> "')")
