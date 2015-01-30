{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Doc where

import qualified Radio              as R
import qualified Data.Text.Lazy     as T
import qualified Data.Text          as ST
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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson         as Ae
import qualified Data.Aeson.Encode  as AeE
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.List.NonEmpty as NEL

type Message = T.Text

data DocF a d = Doc { unDoc :: US (d, Message -> Maybe a) }

type Doc d a = DocF a d

data Element = GUICircles [GUICircle]
             | Button GUIButton
             | TextEntry GUITextEntry
             | Select GUISelect

type US = St.State Int

runUS :: US a -> a
runUS = flip St.evalState 0

unique :: US T.Text
unique = do
  i <- St.get
  St.modify (+1)
  (return . T.pack . show) i

instance Functor (DocF a) where
  fmap = mapDoc

instance Applicative (DocF a) where
  pure x = Doc (pure (x, const Nothing))
  Doc tf <*> Doc tt = Doc (liftA2 (<**>) tf tt)
    where (df, ff) <**> (dx, fx) = (df dx, liftA2 firstJust ff fx)

mapDoc :: (a -> b) -> DocF e a -> DocF e b
mapDoc f (Doc t) = Doc (L.over (L.mapped.L._1) f t)

mapWidgetDoc :: (a -> b) -> (r -> DocF e a) -> (r -> DocF e b)
mapWidgetDoc = fmap . mapDoc

fmapResponse :: (a -> b) -> Doc d a -> Doc d b
fmapResponse f (Doc t) = Doc (L.over (L.mapped.L._2.L.mapped.L.mapped) f t)

fmapNewState :: (a -> b) -> Doc d (e, a) -> Doc d (e, b)
fmapNewState = fmapResponse . L.over L._2

data GUICircle = GUICircle { gcName  :: T.Text
                           , gcColor :: T.Text } deriving Show

data GUIButton = GUIButton { gbName :: T.Text
                           , gbText :: T.Text }

data GUITextEntry = GUITextEntry { gtName     :: T.Text
                                 , gtText     :: T.Text
                                 , gtFocused  :: Bool
                                 , gtPosition :: Int }

data GUISelect = GUISelect { gsName    :: T.Text
                           , gsRadio   :: R.Radio T.Text T.Text
                           , gsFocused :: Bool }

static :: a -> DocF e ()
static = const (pure ())

vert :: Doc [Element] a -> Doc [Element] a -> Doc [Element] a
vert = liftA2 (++)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing (Just b) = Just b
firstJust Nothing Nothing = Nothing

handleMessage :: Doc d a -> Message -> Maybe a
handleMessage (Doc t) = snd (runUS t)

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
                                           Select t      -> selectHtml t
                in (html >> H.br, js)

textEntryHtml :: GUITextEntry -> (H.Html, [T.Text])
textEntryHtml t = (html, js) where
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

selectHtml :: GUISelect -> (H.Html, [T.Text])
selectHtml s = (html, js) where
  html = do
    H.select ! (AH.size . B.toValue . max 2 . length . entries) s
             ! AH.id (B.toValue theId)
             ! AH.onchange (B.toValue ("choose_('" <> B.toValue (gsName s) <> "',this.selectedIndex)")) $ do
      mapM_ (H.option . H.toHtml) (entries s)

  js = (if gsFocused s
        then [ "document.getElementById(\"" <> theId <> "\").focus()" ]
        else []) <>
       ["document.getElementById(\"" <> theId <> "\").selectedIndex = "
         <> (T.pack . show . R.chosenIndex . gsRadio) s ]

  theId = "id" <> gsName s

  entries :: GUISelect -> [T.Text]
  entries = NEL.toList . R.radioToNEL . gsRadio


renderElements :: Doc [Element] a -> T.Text
renderElements (Doc t) = (renderElements' . fst . runUS) t

renderElements' :: [Element] -> T.Text
renderElements' t = (embracket
                     . encodeObject
                     . HashMap.fromList) [("html", html), ("js", js)]
  where total = map elementHtml t
        html = (renderHtml . mapM_ fst) total
        js = (T.intercalate ";\n" . concatMap snd) total
        embracket x = "(" <> x <> ")"

elementOfCircles :: Doc [GUICircle] a -> Doc [Element] a
elementOfCircles = fmap (return . GUICircles)

encodeObject :: HashMap.HashMap ST.Text T.Text -> T.Text
encodeObject = TB.toLazyText
               . AeE.encodeToTextBuilder
               . Ae.toJSON
               . Ae.Object
               . HashMap.map (Ae.String . T.toStrict)

widgetHandler :: (ev -> b -> a) -> (b -> Doc d ev) -> b -> Doc d (ev, a)
widgetHandler f w x = fmapResponse (\ev -> (ev, f ev x)) (w x)
