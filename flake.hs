{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forever, when)
import           Data.Bits (shiftL, xor)
import qualified Data.ByteString as B
import           Data.ByteString.Char8 ()
import qualified Data.Unique as U
import           Data.Word (Word64)
import           Foreign.C.Types (CInt(..), CLong)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.Storable (Storable(..))
import qualified Network.Info as NI
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import qualified System.ZMQ3 as ZMQ

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

intToBase62 :: Integer -> B.ByteString
intToBase62 =
  B.unfoldr toChar
  where
    toChar n
      | n == 0    = Nothing
      | otherwise = return (B.index chars62 (fromIntegral r), q)
      where (q, r) = quotRem n 62
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

macToInt :: NI.MAC -> Int
macToInt (NI.MAC a b c d e f) = foldl (\acc x -> (shiftL acc 8) `xor` (fromIntegral x :: Int)) 0 [a, b, c, d, e, f]   


genId :: MicroTime -> NI.MAC -> U.Unique -> B.ByteString
genId t m u =
    let timestamp = toInteger t
        shifted  = timestamp `shiftL` 32
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
        currentTime <- getMicroTime
        unique <- U.newUnique
        let flake = genId currentTime m unique 
        ZMQ.send s [] flake
