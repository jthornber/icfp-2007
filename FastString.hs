module FastString ( FastString (..)
                  , simplify
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
                  , tails
                  , search
                  , drop
                  , index
                  , range
                  , FastString.splitAt
                  , FastString.getContents
                  , FastString.readFile
                  , FastString.putStr
                  , FastString.putStrLn
                  , draw
                  ) where

-- String type optimised for copying, concatenation and searching

import Prelude hiding (length, tail, head, concat, take, drop)

import Control.Applicative ((<$>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Char
import Data.Int
import qualified Data.List as L

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Maybe

data FastString = Packed !ByteString
                | Simple (Seq Char)
                | Cat !Int !FastString !FastString
                | Range !Int !Int !FastString

-- use these wrapper functions around the constructors so
-- we keep the strings in as simple a form as possible
mkPacked :: ByteString -> FastString
mkPacked = Packed

mkSimple :: Seq Char -> FastString
mkSimple = Simple

mkCat :: FastString -> FastString -> FastString
mkCat (Simple s1) (Simple s2) = Simple $ (S.><) s1 s2
mkCat (Simple s1) (Cat l (Simple s2) s3) = Cat ((S.length s1) + l) (Simple $ (S.><) s1 s2) s3  -- mkCat ?
mkCat (Cat l s1 (Simple s2)) (Simple s3) = Cat ((S.length s3) + l) s1 (Simple $ (S.><) s2 s3)  -- mkCat ?
mkCat s1 s2 = case (length s1, length s2) of
                (0, _) -> s2
                (_, 0) -> s1
                (l1, l2) -> Cat (l1 + l2) s1 s2

-- This top level function stop us making redundant ranges and trims
-- the range to the bounds of the string.
mkRange :: Int -> Int -> FastString -> FastString
mkRange b' e' s =
    let b = max b' 0
        e = max e' 0
    in if e - b == 0
       then empty
       else case length s of
              l -> if b == 0 && e == l
                   then s
                   else if b < l
                        then if e > l
                             then mkRange' b l s
                             else mkRange' b e s
                        else empty

mkRange' :: Int -> Int -> FastString -> FastString
mkRange' 0 e (Simple s) = mkSimple $ S.take e s
-- try to push ranges below cats, so we can better merge them
mkRange' b e (Cat _ s1 s2) = case length s1 of
                              l -> if e <= l
                                   then mkRange b e s1
                                   else if b >= l
                                        then mkRange (b - l) (e - l) s2
                                        else mkCat (mkRange b l s1) (mkRange 0 (e - l) s2)
mkRange' b e (Range b2 _ s) = Range (b2 + b) (b2 + e) s
mkRange' b e s = Range b e s

rebuildSize :: Int
rebuildSize = 10240

rebuild :: FastString -> FastString -> FastString
rebuild orig new = case length orig of
                     l -> if l < rebuildSize
                          then subStr 0 l $ orig
                          else new

simplify :: FastString -> FastString
simplify p@(Packed _) = p
simplify s@(Simple _) = s
simplify c@(Cat _ s1 s2) = rebuild c $ case (simplify s1, simplify s2) of
                                         (s1', s2') -> mkCat s1' s2'
simplify r@(Range b e s) = rebuild r $ case simplify s of
                                        s' -> Range b e s'

subStr :: Int -> Int -> FastString -> FastString
subStr b e = Simple . S.fromList . subStr' b e

subStr' :: Int -> Int -> FastString -> String
subStr' b e = if b < 0 then subSeq' 0 e else subSeq' b e
    where
      subSeq' b' e' s = if b' <= e'
                        then map (fromJust . index s) [b'..(e' - 1)]
                        else const [] s

quote :: String -> String
quote s = '\"' : s ++ "\""

instance Show FastString where
    show = quote . unpack

-- FIXME: slow, use the underlying bytestring compare since that uses
-- memcpy
instance Eq FastString where
    s1 == s2 = case (length s1, length s2) of
                 (len1, len2) -> case len1 - 1 of
                                   l -> if len1 /= len2
                                        then False
                                        else if len1 == 0
                                             then True
                                             else and $ zipWith (==) [index s1 x | x <- [0..l]]
                                                                     [index s2 x | x <- [0..l]]

singleton :: Char -> FastString
singleton = mkSimple . S.singleton

cons :: Char -> FastString -> FastString
cons c b@(Packed _) = mkCat (singleton c) b
cons c (Simple s) = mkSimple $ (S.<|) c s
cons c (Cat _ s1 s2) = mkCat (cons c s1) s2
cons c r = mkCat (singleton c) r

snoc :: FastString -> Char -> FastString
snoc (Simple s) c = mkSimple $ (S.|>) s c
snoc (Cat _ s1 s2) c = mkCat s1 (snoc s2 c)
snoc r c = mkCat r (singleton c)

length :: FastString -> Int
length (Packed s) = B.length s
length (Simple s) = S.length s
length (Cat l _ _) = l
length (Range b e _) = e - b

empty :: FastString
empty = mkSimple S.empty

tail :: FastString -> FastString
tail p@(Packed s) = mkRange 1 (B.length s) p
tail q@(Simple s) = case S.length s of
                      l -> if l <= 1
                           then empty
                           else mkRange 1 l q
tail (Cat _ s1 s2) = case tail s1 of
                       s1' -> if isEmpty s1'
                              then s2
                              else mkCat s1' s2
tail (Range b e s) = if b == e
                     then error "attempt to take tail of empty string"
                     else mkRange (b + 1) e s

head :: FastString -> Maybe Char
head (Packed s) = if B.length s > 0
                  then Just $ B.head s
                  else Nothing
head (Simple s) = if S.length s > 0
                  then Just $ S.index s 0
                  else Nothing
head (Cat _ s _) = head s
head (Range b e s) = if b == e
                     then Nothing
                     else index s b

append :: FastString -> FastString -> FastString
append = mkCat

pack :: String -> FastString
pack str = mkPacked $ B.pack str

unpack :: FastString -> String
{-
unpack (Packed s) = trace ("unpack packed " ++ (show $ B.length s)) $ B.unpack s
unpack (Simple s) = trace "unpack simple" $ F.foldr (:) [] s
unpack (Cat s1 s2) = trace "unpack cat" $ (unpack s1) ++ (unpack s2)
unpack (Range b e s) = trace "unpack range" $ subStr b e s
-}
unpack s = case length s - 1 of
             l -> [fromJust $ index s x | x <- [0..l]]

concat :: [FastString] -> FastString
concat = foldr append empty

take :: Int -> FastString -> FastString
take n (Simple s) = mkSimple $ S.take n s
take n s = mkRange 0 n s

tails :: FastString -> [FastString]
tails fs = (takeWhile (not . isEmpty) . iterate tail $ fs) ++ []

-- FIXME: this can be made a lot faster!
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
drop 0 s = s
drop n (Range b e s) = case e - b of
                         len -> if len > n
                                then mkRange (b + n) e s
                                else empty
drop n s = case length s of
             e -> if e > n
                  then mkRange n e s
                  else empty

index :: FastString -> Int -> Maybe Char
index _ n | n < 0 = Nothing
index s 0 = head s
index (Packed s) n = if B.length s > n
                     then Just $ B.index s n
                     else Nothing
index (Simple s) n = if S.length s > n
                     then Just $ S.index s n
                     else Nothing
index (Cat _ s1 s2) n = case length s1 of
                          e1 -> if e1 > n
                                then index s1 n
                                else index s2 (n - e1)
index (Range b e s) n = if (e - b) > n
                        then index s (b + n)
                        else Nothing

range :: Int -> Int -> FastString -> FastString
range = mkRange

splitAt :: Int -> FastString -> (FastString, FastString)
splitAt n s = (take n s, drop n s)

getContents :: IO FastString
getContents = do
  txt <- B.getContents
  return $ mkPacked txt

readFile :: FilePath -> IO FastString
readFile path = mkPacked <$> B.readFile path

putStr :: FastString -> IO ()
putStr = Prelude.putStr . unpack

putStrLn :: FastString -> IO ()
putStrLn s = FastString.putStr s >> Prelude.putStr "\n"

isEmpty :: FastString -> Bool
isEmpty (Cat _ _ _) = False
isEmpty s = length s == 0

----------------------------------------------------------------
-- Debug aids
draw :: FastString -> String
draw = unlines . draw'

draw' :: FastString -> [String]
draw' (Packed s) = ["packed: " ++ (show $ B.length s)]
draw' (Simple s) = ["simple: " ++ (show $ S.length s)]
draw' (Cat _ s1 s2) = ("cat: (" ++ (show $ depth s1) ++ ", " ++ (show $ depth s2) ++ ")") : indent ((draw' s1) ++ (draw' s2))
draw' (Range b e s) = ["range: " ++ (show b) ++ " " ++ (show e)] ++
                      (indent $ draw' s)

depth :: FastString -> Int
depth (Packed _) = 1
depth (Simple _) = 1
depth (Cat _ s1 s2) = (max (depth s1) (depth s2)) + 1
depth (Range _ _ _) = 1

indent :: [String] -> [String]
indent = map ("  " ++)
