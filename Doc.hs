{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Doc where

import qualified Data.Text.Lazy     as T
import           Data.Monoid        ((<>))
import qualified Text.Blaze         as B
import           Text.Blaze.Html5   ((!))
import qualified Text.Blaze.Html5   as H
import qualified Text.Blaze.Html5.Attributes as AH
import qualified Text.Blaze.Svg11   as S
import qualified Text.Blaze.Svg11.Attributes as AS
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Control.Applicative (liftA2)

type Message = T.Text

data Doc d a = Doc d (Message -> Maybe a)

data Element = GUICircles [GUICircle]
             | Button T.Text

instance Functor (Doc d) where
  fmap f (Doc cs h) = Doc cs ((fmap . fmap) f h)

type Canvas a = Doc [GUICircle] a

data GUICircle = GUICircle { gcName  :: T.Text
                           , gcColor :: T.Text } deriving Show

nullCanvas :: Canvas a
nullCanvas = Doc [] (const Nothing)

horiz :: Canvas a -> Canvas a -> Canvas a
horiz (Doc xs xh) (Doc ys yh) = Doc (xs ++ ys) (liftA2 firstJust xh yh)

vert :: Doc [Element] a -> Doc [Element] a -> Doc [Element] a
vert  (Doc xs xh) (Doc ys yh) = Doc (xs ++ ys) (liftA2 firstJust xh yh)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing (Just b) = Just b
firstJust Nothing Nothing = Nothing

handleMessage :: Canvas a -> Message -> Maybe a
handleMessage (Doc _ h) = h

circlesSvg :: [GUICircle] -> S.Svg
circlesSvg cs = (documentSvg h w . sequence_ . package [0..]) cs
  where package = zipWith (\i c -> circleSvg (50 + i * 100) 50 (B.toValue (gcColor c)) (B.toValue (gcName c)))
        w = 100 * length cs
        h = 100

render :: Canvas a -> T.Text
render (Doc cs _) = renderHtml (circlesSvg cs)

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
           ! AS.onmouseover (handler "mouseover")
           ! AS.onmouseout  (handler "mouseout")
           ! AS.onclick     (handler "click")
    where handler h = h <> "('" <> name <> "')"

buttonHtml :: T.Text -> H.Html
buttonHtml = (H.button ! AH.type_ "button") . H.toHtml

elementHtml :: Element -> H.Html
elementHtml = \case GUICircles gs -> circlesSvg gs
                    Button t      -> buttonHtml t

renderElements :: Doc [Element] a -> T.Text
renderElements (Doc d _) = (renderHtml . mapM_ elementHtml) d
