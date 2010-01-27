{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import FastString (FastString)
import qualified FastString as F

tests :: FastString -> [Benchmark]
tests txt = [ bench "length" $ \(n :: Int) -> F.length txt + n
            , bench "take 10000" $ \n -> F.take (n + 10000 - n) txt
            , bench "length, prepend, sub string" $ \n ->
                F.length ((F.take 100000 $ F.drop 1000 txt) `F.append` txt) + n
            ]

main = do
  txt <- F.readFile "dna/endo.dna"
  defaultMain $ tests txt
       