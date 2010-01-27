{-# LANGUAGE BangPatterns #-}

module FastString3 ( FastString
                   , singleton
                   , cons
                   , snoc
                   , length
                   , empty
                   , tail
                   , head
                   , append
                   , pack
                   , unpack
                   , concat
                   , take
                   , search
                   , drop
                   , index
                   , range
                   , FastString3.splitAt
                   , FastString3.getContents
                   , FastString3.readFile
                   , FastString3.putStr
                   , FastString3.putStrLn
                   ) where

-- String type optimised for copying, concatenation and searching

import Prelude hiding (length, tail, head, concat, take, drop)

import Control.Applicative hiding (empty)
import Data.Char

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import qualified Data.Foldable as F

import Data.Int
import qualified Data.List as L
import Data.Monoid
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import Debug.Trace
import Maybe

----------------------------------------------------------------

data StringElt = SSeq !(Seq Char)
               | SRange !Int !Int !ByteString
                 deriving (Show)

newtype FastString = FString (Seq StringElt)

instance Show FastString where
    show (FString s) = show s

instance Eq FastString where
    s1 == s2 = unpack s1 == unpack s2

singleton :: Char -> FastString
singleton = FString . singleton'

singleton' :: Char -> Seq StringElt
singleton' = S.singleton . SSeq . S.singleton

cons :: Char -> FastString -> FastString
cons c (FString s) = trace "cons" $ FString newS
    where
      newS = case S.viewl s of
               S.EmptyL        -> singleton' c
               (SSeq ss) :< s' -> (SSeq (c <| ss)) <| s'
               _               -> (singleton' c) >< s

snoc :: FastString -> Char -> FastString
snoc (FString s) c = trace "snoc" $ FString $ newS
    where
      newS = case S.viewr s of
               EmptyR          -> singleton' c
               s' :> (SSeq ss) -> s' |> (SSeq (ss |> c))
               _               -> s >< (singleton' c)

length :: FastString -> Int
length (FString s) = trace "length" $ getSum $ F.fold (length' <$> s)
    where
      length' (SSeq ss) = Sum $ S.length ss
      length' (SRange b e _) = Sum $ e - b

empty :: FastString
empty = FString S.empty

tail :: FastString -> FastString
tail = trace "tail" $ drop 1

head :: FastString -> Maybe Char
head (FString s)
    | S.null s = trace "head n" $ Nothing
    | otherwise = trace "head s" $ case S.viewl s of
                    EmptyL               -> Nothing
                    (SSeq ss) :< _       -> Just $ S.index ss 0
                    (SRange b _ bs) :< _ -> Just $ B.index bs b

append :: FastString -> FastString -> FastString
append f1@(FString s1) f2@(FString s2)
       | S.null s1 = f2
       | S.null s2 = f1
       | otherwise = FString $
                     case (S.viewr s1, S.viewl s2) of
                       (pre :> (SSeq ss1), (SSeq ss2) :< post) -> (pre |> (SSeq $ ss1 >< ss2)) >< post
                       _                                       -> s1 >< s2

pack :: String -> FastString
pack [] = empty
pack str = trace "pack" $ FString . S.singleton . SRange 0 (L.length str) $ (B.pack str)

unpack :: FastString -> String
unpack (FString s) = trace "unpack" $ F.fold (toString <$> s)

toString :: StringElt -> String
toString (SSeq s) = F.fold ((:[]) <$> s)
toString (SRange b e bs) = B.unpack . B.take (e - b) . B.drop b $ bs

concat :: [FastString] -> FastString
concat = trace "concat" $ foldr append empty

-- FIXME: this will be slow, will perhaps have to write a custom 2-3
-- finger tree that supports ranges.
-- FIXME: duplicate length calculations below
take :: Int -> FastString -> FastString
take n' (FString s') = trace "take" $ FString $ take' n' s'
    where
      take' !n !s
          | n <= 0    = S.empty
          | otherwise = trace ("in take' " ++ show n ++ ", length s = " ++ show (S.length s)) $ 
                        case S.viewl s of
                          EmptyL -> S.empty

                          sq@(SSeq ss) :< r
                              | trace ("ss length = " ++ (show $ S.length ss)) $ S.length ss >= n -> S.singleton . SSeq $ S.take n ss
                              | otherwise        -> sq <| take' (n - S.length ss) r

                          sq@(SRange b e bs) :< r
                              | trace ("e - b = " ++ (show $ e - b)) $ e - b >= n       -> S.singleton $ SRange b (b + n) bs
                              | otherwise        -> trace ("e = " ++ show e ++ ", b = " ++ show b) $  sq <| take' (n - (e - b)) r

drop :: Int -> FastString -> FastString
drop n' (FString s') = trace "drop" $ FString $ drop' n' s'
    where
      drop' !n !s
          | n <= 0    = trace ("drop " ++ show n) s
          | otherwise = trace ("drop " ++ show n ++ ", " ++ show (S.length s) ++ details (FString s)) $ case S.viewl s of
                          EmptyL -> S.empty

                          (SSeq ss) :< r
                              | S.length ss > n -> (SSeq . S.drop n $ ss) <| r
                              | otherwise       -> drop' (n - S.length ss) r

                          (SRange b e bs) :< r
                              | e - b > n -> (SRange (b + n) e bs) <| r
                              | otherwise -> drop' (n - (e - b)) r

index :: FastString -> Int -> Maybe Char
index txt n
    | n < 0     = trace ("index " ++ show n ++ " " ++ details txt) $ Nothing
    | otherwise = trace ("index " ++ show n ++ " " ++ details txt) $ head . drop n $ txt

range :: Int -> Int -> FastString -> FastString
range b e = trace "range" $ take (zf e - zf b) . drop b
    where
      zf = max 0

splitAt :: Int -> FastString -> (FastString, FastString)
splitAt n s = trace "split at" $ (take n s, drop n s)

getContents :: IO FastString
getContents = toFastString <$> B.getContents

readFile :: FilePath -> IO FastString
readFile path = toFastString <$> B.readFile path

toFastString :: ByteString -> FastString
toFastString bs = FString . S.singleton $ SRange 0 (B.length bs) bs

putStr :: FastString -> IO ()
putStr = Prelude.putStr . unpack

putStrLn :: FastString -> IO ()
putStrLn s = FastString3.putStr s >> Prelude.putStr "\n"

search :: FastString -> FastString -> Maybe Int

-- FIXME: very slow, but hopefully correct implementation
search pat s = trace "search" $ L.findIndex (spat `L.isPrefixOf`) . L.tails $ ss
    where spat = unpack pat
          ss = unpack s
{-
search pat str = trace ("searching:" ++ (unpack pat)) $ search' str 0
    where
      search' s@(FString q) !acc
          | S.null q       = Nothing
          | prefix s = Just acc
          | otherwise      = search' (tail s) (acc + 1)

      epat = length pat - 1

      prefix s = and . map (\i -> (index pat i) == (index s i)) $ [0 .. epat]
-}

details :: FastString -> String
details (FString s) = show . L.take 10 . summariseElts $ s

summariseElts :: Seq StringElt -> [String]
summariseElts s = F.fold (toSummary <$> s)
toSummary (SSeq s) = ["seq " ++ (show . S.length $ s)]
toSummary (SRange b e _) = ["range " ++ show b ++ "-" ++ show e]