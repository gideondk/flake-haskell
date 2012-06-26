module Main where

import qualified System.ZMQ3 as ZMQ
import qualified Data.ByteString as SB
import Control.Monad
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent (threadDelay)
import Data.Time.Clock
import System.Time
import Data.Time.Clock.POSIX
import Data.Bits
import Network.Info
import GHC.Word
import Numeric
import Data.Char
import Data.List

import System.IO
import System.Exit
import System.Environment

import Foreign
import Foreign.C

import Data.IORef
import System.IO.Unsafe
import Data.Unique

import Control.Concurrent (threadDelay, forkIO)

type MicroTime = Word64
getMicroTime :: IO MicroTime

data CTimeval = MkCTimeval !CLong !CLong

instance Storable CTimeval where
    sizeOf _    = (sizeOf (undefined :: CLong)) * 2
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s   <- peekElemOff (castPtr p) 0
        mus <- peekElemOff (castPtr p) 1
        return (MkCTimeval s mus)
    poke p (MkCTimeval s mus) = do
        pokeElemOff (castPtr p) 0 s
        pokeElemOff (castPtr p) 1 mus

foreign import ccall unsafe "time.h gettimeofday" gettimeofday :: Ptr CTimeval -> Ptr () -> IO CInt

-- | Get the current POSIX time from the system clock.
getMicroTime = with (MkCTimeval 0 0) $ \ptval -> do
    result <- gettimeofday ptval nullPtr
    if (result == 0)
        then f `fmap` peek ptval
        else fail ("error in gettimeofday: " ++ (show result))
    where f (MkCTimeval a b) = fromIntegral a * 1000000 + fromIntegral b

-- Base 62 conversion
intToBase62 :: Integer -> String
intToBase62 n = go n ""
  where go n cs
          | n == (toInteger 0) = cs
	  | otherwise = go q (c : cs)
          where (q, r) = quotRem n 62
                c = chooseChar62 r
                chooseChar62 n = chars62 `genericIndex` n
                chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


macToInt :: MAC -> Int
macToInt (MAC a b c d e f) = foldl (\acc x -> (shiftL acc 8) `xor` (fromIntegral x :: Int)) 0 [a, b, c, d, e, f]   


genId :: MicroTime -> MAC -> Unique -> String
genId t m u =
    let timestamp = toInteger t
        shifted  = timestamp `shiftL` 32
        mac = toInteger $ (macToInt m) `shiftL` 16
        rruid = toInteger $ hashUnique u `mod` (2 `shiftL` 16)
        flake = shifted + mac + rruid
    in intToBase62 flake

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ do
        hPutStrLn stderr "usage: flake <ethernet_device>"
        exitFailure
    let ethernetDevice = args !! 0
    interfaces <- getNetworkInterfaces
    let m = mac $ head $ filter (\a -> name a == ethernetDevice) interfaces

    ZMQ.withContext 1 $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s "tcp://*:5555"
            loop m s
  where
    loop m s = forever $ do
        msg <- ZMQ.receive s
        currentTime <- getMicroTime
        unique <- newUnique
        let flake = genId currentTime m unique 
        ZMQ.send s [] $ pack flake
