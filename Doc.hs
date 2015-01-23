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
import           Control.Applicative (liftA2, Applicative, pure, (<*>))

type Message = T.Text

data DocF a d = Doc d (Message -> Maybe a)

type Doc d a = DocF a d

data Element = GUICircles [GUICircle]
             | Button GUIButton

instance Functor (DocF a) where
  fmap f (Doc a d) = Doc (f a) d

instance Applicative (DocF a) where
  pure x = Doc x (const Nothing)
  Doc df ff <*> Doc dx fx = Doc (df dx) (liftA2 firstJust ff fx)

fmapResponse :: (a -> b) -> Doc d a -> Doc d b
fmapResponse f (Doc cs h) = Doc cs ((fmap . fmap) f h)

type Canvas a = Doc [GUICircle] a

data GUICircle = GUICircle { gcName  :: T.Text
                           , gcColor :: T.Text } deriving Show

data GUIButton = GUIButton { gbName :: T.Text
                           , gbText :: T.Text }

nullCanvas :: Canvas a
nullCanvas = Doc [] (const Nothing)

horiz :: Canvas a -> Canvas a -> Canvas a
horiz = liftA2 (++)

vert :: Doc [Element] a -> Doc [Element] a -> Doc [Element] a
vert = liftA2 (++)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing (Just b) = Just b
firstJust Nothing Nothing = Nothing

handleMessage :: Doc d a -> Message -> Maybe a
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
           ! AS.onmouseover (handler' "mouseover")
           ! AS.onmouseout  (handler' "mouseout")
           ! AS.onclick     (handler' "click_")
    where handler' = flip handler name

handler :: S.AttributeValue -> S.AttributeValue -> S.AttributeValue
handler h n = h <> "('" <> n <> "')"

buttonHtml :: GUIButton -> H.Html
buttonHtml b = (H.button ! AH.type_ "button"
                         ! AH.onclick (handler "click_" (B.toValue (gbName b))))
                         (H.toHtml (gbText b))

elementHtml :: Element -> H.Html
elementHtml e = do case e of GUICircles gs -> circlesSvg gs
                             Button t      -> buttonHtml t
                   H.br

renderElements :: Doc [Element] a -> T.Text
renderElements (Doc d _) = (renderHtml . mapM_ elementHtml) d

elementOfCircles :: Doc [GUICircle] a -> Doc [Element] a
elementOfCircles (Doc cs f) = Doc [GUICircles cs] f
