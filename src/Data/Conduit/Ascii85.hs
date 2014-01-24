-- |
-- Conduit for encoding ByteString into Ascii85.
--
--     * <http://en.wikipedia.org/wiki/Ascii85>
--
module Data.Conduit.Ascii85 (
    -- * Conduits
    encode,
    decode
    ) where

import Control.Monad (replicateM_)
import Data.ByteString (ByteString, pack)
import Data.Conduit -- (Conduit, Consumer, yield)
import qualified Data.Conduit.Binary as CB
import Data.Bits (shift, (.&.))
import Data.Word (Word8)

-- | Ascii85 encoder.
encode :: Monad m => Conduit ByteString m ByteString
encode = go
  where
    next = CB.head
    go = do
        mx <- next
        case mx of
            Nothing ->
                return ()
            Just x  -> do
                my <- next
                case my of
                    Nothing -> do
                        yield $ pack85 x 0 0 0 2
                        return ()
                    Just y  -> do
                        mz <- next
                        case mz of
                            Nothing -> do
                                yield $ pack85 x y 0 0 3
                                return ()
                            Just z  -> do
                                mw <- next
                                case mw of
                                    Nothing -> do
                                        yield $ pack85 x y z 0 4
                                        return ()
                                    Just w  -> do
                                        yield $ pack85 x y z w 5
                                        go

pack85 :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> ByteString
pack85 x y z w c =
    pack $
    if c == 5 && v == 0
    then [122] -- 'z'
    else map f $ take c
         [ v `div` 85^4 , v `div` 85^3 , v `div` 85^2 , v `div` 85 , v ]
  where
    v = toInteger x * 2^24
        + toInteger y * 2^16
        + toInteger z * 2^8
        + toInteger w
    f n = fromIntegral $ n `mod` 85 + 33

unpack85 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Int -> ByteString
unpack85 x y z w v c =
    pack $ map f $ take c
    [ n `shift` (-24), n `shift` (-16), n `shift` (-8), n ]
  where
    n = toInteger x * 85^4
        + toInteger y * 85^3
        + toInteger z * 85^2
        + toInteger w * 85
        + toInteger v
    f = fromIntegral . (255 .&.)

u :: Word8
u = 117

flatten :: Monad m => Conduit ByteString m Word8
flatten = go
  where
    next = CB.head
    go = do
        mx <- next
        case mx of
            Just x
              | x < 33    -> go
              | x == 122  -> do
                  replicateM_ 5 $ yield 0
                  go
              | u < x     -> return ()
              | otherwise -> do
                  yield $ x - 33
                  go
            Nothing       -> return ()

-- | Ascii85 decoder.
decode :: Monad m => Conduit ByteString m ByteString
decode = flatten =$= go
  where
    next = await
    go = do
        mx <- next
        case mx of
            Nothing ->
                return ()
            Just x  -> do
                my <- next
                case my of
                    Nothing -> do
                        return ()
                    Just y  -> do
                        mz <- next
                        case mz of
                            Nothing -> do
                                yield $ unpack85 x y u u u 1
                                return ()
                            Just z  -> do
                                mw <- next
                                case mw of
                                    Nothing -> do
                                        yield $ unpack85 x y z u u 2
                                        return ()
                                    Just w  -> do
                                        mv <- next
                                        case mv of
                                            Nothing -> do
                                                yield $ unpack85 x y z w u 3
                                                return ()
                                            Just v  -> do
                                                yield $ unpack85 x y z w v 4
                                                go
