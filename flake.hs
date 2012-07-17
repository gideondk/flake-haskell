{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forever, when)
import           Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as B
import           Data.ByteString.Char8 ()
import           Data.List (foldl')
import qualified Data.Time.Clock.POSIX as CP
import qualified Data.Unique as U
import           Data.Word (Word64)
import qualified Network.Info as NI
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import qualified System.ZMQ3 as ZMQ

msSinceEpoch :: Integral t => IO t
msSinceEpoch =
  (floor . (1000 *)) `fmap` CP.getPOSIXTime

intToBase62 :: Integer -> B.ByteString
intToBase62 =
  B.unfoldr toChar
  where
    toChar n
      | n == 0    = Nothing
      | otherwise = return (B.index chars62 (fromIntegral r), q)
      where (q, r) = quotRem n 62
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

macToInt :: NI.MAC -> Word64
macToInt (NI.MAC a b c d e f) =
  foldl' (\acc byte -> shiftL acc 8 .|. fromIntegral byte) 0 [a, b, c, d, e, f]


genId :: Integer -> NI.MAC -> U.Unique -> B.ByteString
genId t m u =
    let shifted  = t `shiftL` 32
        mac = toInteger $ (macToInt m) `shiftL` 16
        rruid = toInteger $ U.hashUnique u `mod` (2 `shiftL` 16)
        flake = shifted + mac + rruid
    in intToBase62 flake

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ do
        hPutStrLn stderr "usage: flake <ethernet_device>"
        exitFailure
    let ethernetDevice = args !! 0
    interfaces <- NI.getNetworkInterfaces
    let m = NI.mac $ head $ filter (\a -> NI.name a == ethernetDevice) interfaces

    ZMQ.withContext 1 $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s "tcp://*:5555"
            loop m s
  where
    loop m s = forever $ do
        _ <- ZMQ.receive s
        currentTime <- msSinceEpoch
        unique <- U.newUnique
        let flake = genId currentTime m unique 
        ZMQ.send s [] flake
