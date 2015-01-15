{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import           Data.Monoid        ((<>))
import qualified Data.IORef         as R

svg :: Bool -> T.Text -> T.Text
svg over m = "<svg width='100' height='100'>\
\  <circle id='bar' cx='50' cy='50' r='40' stroke='green' stroke-width='4' fill='" <> colour <> "' \
\          onmouseover='mouseover(this.id)'\
\          onmouseout='mouseout(this.id)'\
\/>\
\  <p>" <> m <> "</p>"
    where colour = if over then "yellow" else "red"


meow :: R.IORef Int -> WS.PendingConnection -> IO ()
meow r pc = do
  conn <- WS.acceptRequest pc
  forever $ do
    msg  <- WS.receiveData conn
    n    <- R.readIORef r
    let over = msg == "over"
    R.writeIORef r (n + 1)
    WS.sendTextData conn (svg over (msg <> T.pack (show n)))

main :: IO ()
main = do
  r <- R.newIORef 0
  WS.runServer "0.0.0.0" 9998 (meow r)
