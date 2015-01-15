{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Control.Lens       as L

data Circle = Circle { _cName  :: T.Text
                     , _cColor :: T.Text } deriving Show
$(L.makeLenses ''Circle)

type Message = T.Text

data Canvas a = Canvas [Circle] (Message -> Maybe a)

instance Functor Canvas where
  fmap f (Canvas cs h) = Canvas cs ((fmap . fmap) f h)

data CircleEvent = MouseOver | MouseOut deriving Show

circleHandle :: CircleEvent -> Circle -> Circle
circleHandle MouseOver = L.set cColor "yellow" 
circleHandle MouseOut  = L.set cColor "red" 

circle :: Circle -> Canvas CircleEvent
circle (Circle name color) = Canvas [Circle { _cName = name, _cColor = color }]
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
  
  where package = zipWith (\c i -> circleSvg (50 + i * 100) 50 (B.toValue (_cColor c)) (B.toValue (_cName c)))

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

  let initialGui = (Circle "id1" "yellow", Circle "id2" "blue")

      makeCanvas (left, right) = fmap (\ev -> (circleHandle ev left, right)) (circle left)
                                 `horiz`
                                 fmap (\ev -> (left, circleHandle ev right)) (circle right)

  let loop canvas = do
        msg  <- WS.receiveData conn
        n    <- R.readIORef r
        
        let mNextGui   = handleMessage canvas msg
            nextCanvas = maybe canvas makeCanvas mNextGui
  
        print msg
        print mNextGui

        R.writeIORef r (n + 1)
        WS.sendTextData conn (render (nextCanvas))
  
        loop nextCanvas

  loop (makeCanvas initialGui)


main :: IO ()
main = do
  r <- R.newIORef 0
  WS.runServer "0.0.0.0" 9998 (meow r)
