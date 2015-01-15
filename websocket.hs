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

data Circle = Circle { cName :: T.Text
                     , cColor :: S.AttributeValue }

type Message = T.Text

data Canvas a = Canvas [Circle] (Message -> Maybe a)

data CircleEvent = MouseOver | MouseOut deriving Show

circle :: T.Text -> S.AttributeValue -> Canvas CircleEvent
circle name color = Canvas [Circle { cName = name, cColor = color }]
                           (\message -> case T.split (== ',') message
                                        of [theName, theEvent] ->
                                             if theName == name
                                             then case theEvent
                                                  of "mouseover" -> Just MouseOver
                                                     "mouseout"  -> Just MouseOut
                                                     _           -> Nothing
                                             else Nothing
                                           _ -> Nothing)

horiz :: Canvas a -> Canvas a -> Canvas a
horiz (Canvas xs xh) (Canvas ys yh) = Canvas (xs ++ ys)
                                             (\message -> case xh message of
                                                 r@(Just _) -> r
                                                 Nothing -> case yh message of
                                                   s@(Just _) -> s
                                                   Nothing -> Nothing)                                                          

handleMessage :: Canvas a -> Message -> Maybe a
handleMessage (Canvas _ h) = h

render :: Canvas a -> T.Text
render (Canvas cs _) = renderHtml $ S.svg ! AS.width (B.toValue (100 * length cs))
                                          ! AS.height "100" $ do
  sequence_ (package cs [0..])
  
  where package = zipWith (\c i -> circleSvg (50 + i * 100) 50 (cColor c) (B.toValue (cName c)))

circleSvg :: Int -> Int -> S.AttributeValue -> S.AttributeValue -> S.Svg
circleSvg cx cy color name = 
  S.circle ! AS.cx (B.toValue cx)
           ! AS.cy (B.toValue cy)
           ! AS.r "40"
           ! AS.stroke "black"
           ! AS.strokeWidth "4"
           ! AS.fill color
           ! AS.onmouseover ("mouseover('" <> name <> "')")
           ! AS.onmouseout ("mouseout('" <> name <> "')")

meow :: R.IORef Int -> WS.PendingConnection -> IO ()
meow r pc = do
  conn <- WS.acceptRequest pc

  let canvas = circle "id1" "yellow" `horiz` circle "id2" "blue"

  forever $ do
    msg  <- WS.receiveData conn
    n    <- R.readIORef r
    
    let ev = handleMessage canvas msg

    print msg
    print ev

    R.writeIORef r (n + 1)
    WS.sendTextData conn (render canvas)

main :: IO ()
main = do
  r <- R.newIORef 0
  WS.runServer "0.0.0.0" 9998 (meow r)
