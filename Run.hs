module Run where

import qualified Doc3               as D3
import qualified Doc                as D
import qualified Network.WebSockets as WS
import qualified Filter             as F
import qualified Focus              as Focus

run' :: (d -> IO D.Message) -> (s -> D3.Doc a s d) -> s -> IO s
run' f fd s = do
  let D3.Doc (D3.DocP u) = fd s
      (D3.ReadMessage mab, d) = D.runUS u
  message <- f d
  (return . Focus.mostFocused . mab) message

run :: (d -> IO D.Message) -> (s -> D3.Doc e s d) -> s -> IO void
run f fd s = do
  s' <- run' f fd s
  run f fd s'

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = F.filterMake

      gui = D3.filterC

      handler d = do
        WS.sendTextData conn (D.renderElements' d)
        msg <- WS.receiveData conn
        print msg
        return msg

  run handler gui initialGui

main :: IO ()
main = WS.runServer "0.0.0.0" 9998 runServer
