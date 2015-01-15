{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import           Data.Monoid        ((<>))
import qualified Data.IORef         as R

svg :: T.Text -> T.Text
svg m = "<svg width='100' height='100'>\
\  <circle id='bar' cx='50' cy='50' r='40' stroke='green' stroke-width='4' fill='yellow' \
\          onmouseover='mouseover(this.id)'\
\          onmouseout='mouseout(this.id)'\
\/>\
\  <p>" <> m <> "</p>"

meow :: R.IORef Int -> WS.PendingConnection -> IO ()
meow r pc = forever $ do
  conn <- WS.acceptRequest pc
  msg  <- WS.receiveData conn
  n    <- R.readIORef r
  R.writeIORef r (n + 1)
  WS.sendTextData conn (svg (msg <> T.pack (show n)))

main :: IO ()
main = do
  r <- R.newIORef 0
  WS.runServer "0.0.0.0" 9998 (meow r)
