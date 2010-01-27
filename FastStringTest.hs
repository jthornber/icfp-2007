{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables, TypeSynonymInstances #-}

module Main where

import FastString3 (FastString)
import qualified FastString3 as FS

import Control.Applicative
import Data.Maybe
import qualified Data.List as L
import Prelude hiding (replicate)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck

----------------------------------------------------------------

instance Arbitrary FastString where
    arbitrary = FS.pack <$> (listOf $ choose ('a', 'z'))

last :: FastString -> Char
last = L.last . FS.unpack

pack_unpack txt = (FS.unpack . FS.pack $ txt) == txt
packed_length txt = (FS.length . FS.pack $ txt) == length txt
length_singleton c = (FS.unpack . FS.singleton $ c) == [c]
cons_increases_length_by_one c txt = (FS.length . FS.cons c $ txt) == FS.length txt + 1
head_cons c txt = (FS.head . FS.cons c $ txt) == Just c
head_null = (FS.head . FS.pack $ []) @?= Nothing
head_non_null txt = length txt > 0 ==> isJust . FS.head . FS.pack $ txt
snoc_length c txt = (FS.length . FS.snoc txt $ c) == FS.length txt + 1
snoc_last c txt = (Main.last . FS.snoc txt $ c) == c
sum_length t1 t2 = (FS.length $ t1 `FS.append` t2) == FS.length t1 + FS.length t2
empty_has_zero_length = FS.length FS.empty @?= 0
cons_tail_is_id c txt = (FS.tail . FS.cons c $ txt) == txt
tail_empty_is_empty = FS.tail FS.empty @?= FS.empty
concat_is_foldr_append ts = (foldr FS.append FS.empty ts) == FS.concat ts
takes_enough n txt = let l = FS.length . FS.take n $ txt
                     in l == (max n 0) || l == FS.length txt
take_append_id t1 t2 = (FS.take (FS.length t1) (FS.append t1 t2)) == t1
drop_append_id t1 t2 = (FS.drop (FS.length t1) (FS.append t1 t2)) == t2
drop_length n t = let l = FS.length . FS.drop n $ t
                  in l + (max n 0) == FS.length t || l == 0
range_length n1 n2 t = let n1' = min n1 n2
                           n2' = max n1 n2
                           l = FS.length . FS.range n1' n2' $ t
                           o = FS.length t
                       in if n1' < o
                          then if n2' <= o
                               then l == (max n2' 0) - (max n1' 0)
                               else l == o - (max n1' 0)
                          else l == 0
range_is_drop_take n1 n2 t = n2 >= n1 ==> 
                             (FS.range n1 n2 t) == (FS.pack . take ((max n2 0) - (max n1 0)) . drop n1 . FS.unpack $ t)
index_works n t = n < FS.length t && n > 0 ==>
                  (FS.index t n) == Just (head . drop n . FS.unpack $ t)
index_below n t = n < 0 ==>
                  FS.index t n == Nothing
index_above n t = n >= FS.length t ==>
                  FS.index t n == Nothing

----------------------------------------------------------------

main = defaultMain tests

tests = [ testGroup "general"
                        [ testProperty "pack unpack == id" pack_unpack
                        , testProperty "packed length" packed_length
                        , testProperty "length singleton == 1" length_singleton
                        , testProperty "cons increases length by one" cons_increases_length_by_one
                        , testProperty "head cons is original char" head_cons
                        , testCase "head of null string is Nothing" head_null
                        , testProperty "head non null string is a Just" head_non_null
                        , testProperty "length snoc == +1" snoc_length
                        , testProperty "last snoc == original" snoc_last
                        , testProperty "sum length" sum_length
                        , testCase "length of empty is zero" empty_has_zero_length
                        , testProperty "cons tail is id" cons_tail_is_id
                        , testCase "tail of empty is empty" tail_empty_is_empty
                        , testProperty "concat is foldr append" concat_is_foldr_append
                        , testProperty "result of take is long enough" takes_enough
                        , testProperty "take append is id" take_append_id
                        , testProperty "drop append is id" drop_append_id
                        , testProperty "drop length" drop_length
                        , testProperty "range length" range_length
                        , testProperty "range is drop take" range_is_drop_take
                        , testProperty "index works" index_works
                        , testProperty "index oob below" index_below
                        , testProperty "index oob above" index_above
                        ]
        ]
