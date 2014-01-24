{-# LANGUAGE OverloadedStrings #-}

module Ascii85Spec where

import Data.ByteString (ByteString, append)
import Data.Conduit
import Data.Conduit.Ascii85
import Test.Hspec

src :: Monad m => ByteString -> Producer m ByteString
src = yield

src2 :: Monad m => ByteString -> Producer m ByteString
src2 x = do
    yield x
    yield x

sink :: Monad m => Consumer ByteString m ByteString
sink = go ""
  where
    go a = do
        ms <- await
        case ms of
            Nothing -> return a
            Just s -> go $ a `append` s

conv :: Conduit ByteString IO ByteString
     -> ByteString -> IO ByteString
conv f input = src input $$ f =$ sink

spec :: Spec
spec =
    describe "encode" $ do
        it "encode empty" $ do
            let i = ""
            e <- conv encode i
            e `shouldBe` ""
            d <- conv decode e
            d `shouldBe` i

        it "encode 1 zero" $ do
            let i = "\NUL"
            e <- conv encode i
            e `shouldBe` "!!"
            d <- conv decode e
            d `shouldBe` i

        it "encode 2 zero" $ do
            let i = "\NUL\NUL"
            e <- conv encode i
            e `shouldBe` "!!!"
            d <- conv decode e
            d `shouldBe` i

        it "encode 3 zero" $ do
            let i = "\NUL\NUL\NUL"
            e <- conv encode i
            e `shouldBe` "!!!!"
            d <- conv decode e
            d `shouldBe` i

        it "encode 4 zero" $ do
            let i = "\NUL\NUL\NUL\NUL !."
            e <- conv encode i
            e `shouldBe` "z+<`?"
            d <- conv decode e
            d `shouldBe` i
            d <- conv decode "!!!!!+<`?"
            d `shouldBe` i

        it "encode 4 zero" $ do
            e <- src2 "\NUL\NUL"  $$ encode =$ sink
            e `shouldBe` "z"

        it "encode a text" $ do
            let i = "Hello, World"
                j = "87cURD_*#4DfTZ)"
                j' = "87cURD_\n*#4DfTZ)"
            e <- conv encode i
            e `shouldBe` j
            d <- conv decode j
            d `shouldBe` i
            d <- conv decode j'
            d `shouldBe` i

        it "encode a quote" $ do
            e <- conv encode quote
            e `shouldBe` encoded
            d <- conv decode e
            d `shouldBe` quote
          where
            quote = -- A quote from Thomas Hobbes's Leviathan:
                "Man is distinguished, not only by his reason,\
                \ but by this singular passion from other animals,\
                \ which is a lust of the mind, that by a perseverance\
                \ of delight in the continued and indefatigable generation\
                \ of knowledge, exceeds the short vehemence of any carnal\
                \ pleasure."
            encoded =
                "9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)\
                \/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.\
                \Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U\
                \=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlo\
                \lDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JM\
                \K@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)\
                \oF*2M7/c"
