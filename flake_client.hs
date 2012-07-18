module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf


import System.ZMQ3
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = withContext 1 $ \c -> withSocket c Req act

act :: (Receiver a, Sender a) => Socket a -> IO ()
act sock = do
  connect sock "tcp://localhost:5555"
  begin <- getCurrentTime
  putStrLn "Requesting 10000 keys"
  flip mapM_ ([1..10000] :: [Int]) $ \i -> do
    let msg = show i
    send sock [] $ pack msg 
    recv <- receive sock
    return recv
  end <- getCurrentTime
  let diff = realToFrac $ diffUTCTime end begin :: Double
  putStrLn $ printf "Time (sec): %f" diff
  putStrLn "Done"
     
