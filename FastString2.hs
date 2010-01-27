module FastString2 ( FastString (..)
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
                   , FastString2.splitAt
                   , FastString2.getContents
                   , FastString2.putStr
                   , FastString2.putStrLn
                   ) where

-- String type optimised for copying, concatenation and searching

import Prelude hiding (length, tail, head, concat, take, drop)

import Data.Char
import Data.Int
import qualified Data.List as L

import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as S

import Maybe

----------------------------------------------------------------

newtype FastString = FastString (Seq Char)

instance Show FastString where
    show (FastString s) = show s

instance Eq FastString where
    (FastString s1) == (FastString s2) = s1 == s2

singleton :: Char -> FastString
singleton = FastString . S.singleton

cons :: Char -> FastString -> FastString
cons c (FastString s) = FastString $ c <| s

snoc :: FastString -> Char -> FastString
snoc (FastString s) c = FastString $ s |> c

length :: FastString -> Int
length (FastString s) = S.length s

empty :: FastString
empty = FastString S.empty

tail :: FastString -> FastString
tail (FastString s) = FastString $ S.drop 1 s

head :: FastString -> Maybe Char
head (FastString s) = if S.null s then Nothing else Just $ S.index s 0

append :: FastString -> FastString -> FastString
append (FastString s1) (FastString s2) = FastString $ s1 >< s2

pack :: String -> FastString
pack str = FastString $ S.fromList str

unpack :: FastString -> String
unpack (FastString s) =
    if S.null s then [] else S.index s 0 : FastString2.unpack (FastString $ S.drop 1 s)

concat :: [FastString] -> FastString
concat = foldr append empty

take :: Int -> FastString -> FastString
take n (FastString s) = FastString $ S.take n s

search :: FastString -> FastString -> Maybe Int
search pat str = 
    case (length pat, length str) of
      (lpat, lstr) -> L.find (match lpat) [0..(lstr - lpat)]

    where
      match :: Int -> Int -> Bool
      match lpat n = and .
                     map (\i -> (index str (n + i)) == (index pat i)) $
                     [0..(lpat - 1)]

drop :: Int -> FastString -> FastString
drop n (FastString s) = FastString $ S.drop n s

index :: FastString -> Int -> Maybe Char
index (FastString s) n
      | n < 0           = Nothing
      | n >= S.length s = Nothing
      | otherwise       = Just $ S.index s n

range :: Int -> Int -> FastString -> FastString
range b e = take (e' - b') . drop b'
    where
      b' = zf b
      e' = zf e
      zf = max 0

splitAt :: Int -> FastString -> (FastString, FastString)
splitAt n s = (take n s, drop n s)

getContents :: IO FastString
getContents = do
  txt <- Prelude.getContents
  return $ pack txt

putStr :: FastString -> IO ()
putStr = Prelude.putStr . unpack

putStrLn :: FastString -> IO ()
putStrLn s = FastString2.putStr s >> Prelude.putStr "\n"

isEmpty :: FastString -> Bool
isEmpty (FastString s) = S.null s
