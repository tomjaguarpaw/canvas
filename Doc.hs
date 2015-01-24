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
import qualified Control.Monad.Trans.State as St
import qualified Control.Lens       as L

type Message = T.Text

data DocF a d = Doc (US (d, Message -> Maybe a))

type Doc d a = DocF a d

data Element = GUICircles [GUICircle]
             | Button GUIButton
             | TextEntry GUITextEntry

type US = St.State Int

unique :: US T.Text
unique = do
  i <- St.get
  St.modify (+1)
  (return . T.pack . show) i

instance Functor (DocF a) where
  fmap f (Doc t) = Doc (L.over (L.mapped.L._1) f t)

instance Applicative (DocF a) where
  pure x = Doc (pure (x, const Nothing))
  Doc tf <*> Doc tt = Doc (liftA2 (<**>) tf tt)
    where (df, ff) <**> (dx, fx) = (df dx, liftA2 firstJust ff fx)

fmapResponse :: (a -> b) -> Doc d a -> Doc d b
fmapResponse f (Doc t) = Doc (L.over (L.mapped.L._2.L.mapped.L.mapped) f t)

data GUICircle = GUICircle { gcName  :: T.Text
                           , gcColor :: T.Text } deriving Show

data GUIButton = GUIButton { gbName :: T.Text
                           , gbText :: T.Text }

data GUITextEntry = GUITextEntry { gtName     :: T.Text
                                 , gtText     :: T.Text
                                 , gtFocused  :: Bool
                                 , gtPosition :: Int }

horiz :: Doc [GUICircle] a -> Doc [GUICircle] a -> Doc [GUICircle] a
horiz = liftA2 (++)

vert :: Doc [Element] a -> Doc [Element] a -> Doc [Element] a
vert = liftA2 (++)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing (Just b) = Just b
firstJust Nothing Nothing = Nothing

handleMessage :: Doc d a -> Message -> Maybe a
handleMessage (Doc t) = snd (St.evalState t 0)

circlesSvg :: [GUICircle] -> (S.Svg, [a])
circlesSvg cs = ((documentSvg h w . sequence_ . package [0..]) cs, [])
  where package = zipWith (\i c -> circleSvg (50 + i * 100) 50 (B.toValue (gcColor c)) (B.toValue (gcName c)))
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
           ! AS.onmouseover (handler' "mouseover")
           ! AS.onmouseout  (handler' "mouseout")
           ! AS.onclick     (handler' "click_")
    where handler' = flip handler name

handler :: S.AttributeValue -> S.AttributeValue -> S.AttributeValue
handler h n = h <> "('" <> n <> "')"

buttonHtml :: GUIButton -> (H.Html, [a])
buttonHtml b = (html, [])
  where html = (H.button ! AH.type_ "button"
                         ! AH.onclick (handler "click_" (B.toValue (gbName b))))
                 (H.toHtml (gbText b))

elementHtml :: Element -> (H.Html, [T.Text])
elementHtml e = let (html, js) = case e of GUICircles gs -> circlesSvg gs
                                           Button t      -> buttonHtml t
                                           TextEntry t   -> textEntryHtml t
                in (html >> H.br, js)

textEntryHtml :: GUITextEntry -> (H.Html, [T.Text])
textEntryHtml t =  (html, js) where
  html = do
    H.input ! AH.type_ "text"
            ! AH.oninput (B.toValue ("input_('" <> B.toValue (gtName t) <> "',this.value,this.selectionStart)"))
            ! AH.value (B.toValue (gtText t))
            ! AH.id (B.toValue theId)
  js = if gtFocused t
       then [ "document.getElementById(\"" <> theId <> "\").focus()"
            , "document.getElementById(\"" <> theId <> "\").selectionStart = "
              <> T.pack (show (gtPosition t)) ]
       else []
  theId = "id" <> gtName t

renderElements :: Doc [Element] a -> T.Text
renderElements (Doc t) = "({ \"html\" : '" <> html <> "'\n" <>
                         ", \"js\"    : '" <> js <> "' })"
  where total = (map elementHtml . fst . flip St.evalState 0) t
        html = (renderHtml . sequence_ . map fst) total
        js = (T.intercalate ";\\n" . concatMap snd) total

elementOfCircles :: Doc [GUICircle] a -> Doc [Element] a
elementOfCircles = fmap (return . GUICircles)

widgetHandler :: (ev -> b -> a) -> (b -> Doc d ev) -> b -> Doc d (ev, a)
widgetHandler f w x = fmapResponse (\ev -> (ev, f ev x)) (w x)
