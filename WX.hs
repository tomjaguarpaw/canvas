{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.UI.WX hiding (Layout, Button, button, widget, textEntry)
import qualified Doc                as D
import           Doc3               (DocR, contains, also, emitting)
import qualified Doc3               as D3
import           Control.Monad      (guard)
import qualified Data.Dynamic       as Dyn
import qualified Control.Lens       as L
import           Focus              (Focus(NeedFocus,
                                           WantFocus, Don'tWantFocus))
import           WXRender           (Layout, runWX, Layout(Column, Row))
import qualified WXRender           as WXR

data Button = Button { _bText    :: String
                     , _bFocused :: Bool }
$(L.makeLenses ''Button)

data TextEntry = TextEntry { _teText     :: String
                           , _teFocused  :: Bool
                           , _tePosition :: Int }
$(L.makeLenses ''TextEntry)

buttonD :: Button -> D.DocF D3.Message () Layout
buttonD (Button t f) = D.Doc $ do
  n <- D.uniqueInt
  return (WXR.ButtonL t f n, parseMessage n)
  where parseMessage n message = do
          let (mn, _) = message
          guard (n == mn)

mkButton :: String -> Button
mkButton = flip Button False

mkTextEntry :: String -> TextEntry
mkTextEntry s = TextEntry s False 0

button :: Button -> DocR () Button Layout
button = D3.makeDoc (\b -> \case Nothing -> if L.view bFocused b
                                            then WantFocus b
                                                 (L.set bFocused False b)
                                            else Don'tWantFocus b
                                 Just t  -> NeedFocus
                                            t
                                            (L.set bFocused True b)
                                 )
                    buttonD

textEntryD :: TextEntry -> D.DocF D3.Message (String, Int) Layout
textEntryD (TextEntry t f p) = D.Doc $ do
  n <- D.uniqueInt
  return (WXR.TextEntryL t f p n, parseMessage n)
  where parseMessage n message = do
          let (mn, d) = message
          guard (n == mn)
          Dyn.fromDynamic d

textEntry :: TextEntry -> DocR (String, Int) TextEntry Layout
textEntry = D3.makeDoc (\te -> \case Nothing -> if L.view teFocused te
                                                then WantFocus te
                                                     (L.set teFocused False te)
                                                else Don'tWantFocus te
                                     Just s -> NeedFocus
                                               s
                                               ((L.set teFocused True
                                                . L.set teText (fst s)
                                                . L.set tePosition (snd s)) te)
                                     )
                    textEntryD

-- Just like sequence :: Applicative f => [f a] -> f [a]
sequence2 :: (forall a b a' b'. f a a' -> f b b' -> f (a, b) (a', b'))
          -> (forall a b a' b'. (a -> b) -> (a' -> b') -> f a a' -> f b b')
          -> (forall a b. a -> b -> f a b)
          -> [f c d]
          -> f [c] [d]
sequence2 _     _     bipure []     = bipure [] []
sequence2 (***) bimap bipure (x:xs) = bimap (uncurry (:)) (uncurry (:))
                                            (x *** sequence2 (***) bimap bipure xs)

list :: (state -> DocR event state gui) -> [state]
     -> DocR event [state] [gui]
list w = sequence2 D3.pair D3.mapBD D3.bipure . fmap w

main :: IO ()
main = start hello

hello :: IO ()
hello = let widget = \(x1, y1) ->
             ((,), \x y -> Column 1 (map (Row 1) [x, y]))
             `contains` (list button x1)
             `also` (list textEntry y1 `emitting` const ())

            initial = ([mkButton "Hello", mkButton "Something", mkButton "Else"],
                       [mkTextEntry "Foo"])

            in runWX widget initial
