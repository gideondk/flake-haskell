module Main where

import qualified Data.ByteString as B
import           Data.ByteString.Lex.Integral (packDecimal)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Text.Printf
import qualified System.ZMQ3 as ZMQ

main :: IO ()
main = ZMQ.withContext 1 $ \c -> ZMQ.withSocket c ZMQ.Req act

act :: (ZMQ.Receiver a, ZMQ.Sender a) => ZMQ.Socket a -> IO ()
act sock = do
  ZMQ.connect sock "tcp://localhost:5555"
  begin <- getCurrentTime
  putStrLn "Requesting 10000 keys"
  flip mapM_ ([1..10000] :: [Int]) $ \i -> do
    case packDecimal i of
      Just msg -> ZMQ.send sock [] msg 
      Nothing  -> ZMQ.send sock [] B.empty -- Whatever
    recv <- ZMQ.receive sock
    return recv
  end <- getCurrentTime
  let diff = realToFrac $ diffUTCTime end begin :: Double
  putStrLn $ printf "Time (sec): %f" diff
  putStrLn "Done"
     
