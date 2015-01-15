{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad      (forever)
import qualified Data.Text.Lazy     as T
import qualified Network.WebSockets as WS
import           Data.Monoid        ((<>))
import qualified Data.IORef         as R
import qualified Text.Blaze         as B
import qualified Text.Blaze.Html5   as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5   ((!))
import qualified Text.Blaze.Svg11   as S
import qualified Text.Blaze.Svg11.Attributes as AS
import           Text.Blaze.Html.Renderer.Text (renderHtml)

circleSvg :: Int -> Int -> S.AttributeValue -> S.AttributeValue -> S.Svg
circleSvg cx cy color name = S.svg ! AS.width "100" ! AS.height "100" $ do
  S.circle ! AS.id_ "bar" ! AS.cx (B.toValue cx) ! AS.cy (B.toValue cy) ! AS.r "40" ! AS.stroke "green"
           ! AS.strokeWidth "4"
           ! AS.fill color
           ! AS.onmouseover ("mouseover(" <> name <> ")")
           ! AS.onmouseout ("mouseout(" <> name <> ")")

svg :: Bool -> T.Text
svg over = renderHtml $ circleSvg 50 50 color "this.id"
    where color = if over then "yellow" else "red"

meow :: R.IORef Int -> WS.PendingConnection -> IO ()
meow r pc = do
  conn <- WS.acceptRequest pc
  forever $ do
    msg  <- WS.receiveData conn
    n    <- R.readIORef r
    let over = msg == ("over" :: T.Text)
    R.writeIORef r (n + 1)
    WS.sendTextData conn (svg over)

main :: IO ()
main = do
  r <- R.newIORef 0
  WS.runServer "0.0.0.0" 9998 (meow r)
