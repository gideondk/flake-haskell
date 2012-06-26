module Main where

import Foreign
import Foreign.C

import Data.IORef
import System.IO.Unsafe
import Data.Unique

import Control.Concurrent (threadDelay)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.ZMQ3
import Data.ByteString.Char8 (pack, unpack)

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

main :: IO ()
main = withContext 1 $ \c -> withSocket c Req act

act :: (Receiver a, Sender a) => Socket a -> IO ()
act sock = do
	connect sock "tcp://localhost:5555"
	begin <- getMicroTime
	putStrLn "Requesting 10000 keys"
	flip mapM_ ([1..10000] :: [Int]) $ \i -> do
		let msg = show i
		send sock [] $ pack msg 
		recv <- receive sock
		return recv
	end <- getMicroTime
	let diff = (fromIntegral (toInteger end)) - (fromIntegral (toInteger begin))
	putStrLn $ show (diff / 1000 / 1000)
	putStrLn "Done"
		 