{-# LANGUAGE FlexibleContexts, GADTs, CPP #-}

module Main where

import Gauge (bench, bgroup, nf, nfAppIO)
import Gauge.Main (defaultMain)
import qualified Weigh

import qualified Fused.Bench as Fused
import qualified Freer.Bench as Freer
import qualified Shallow.Bench as Shallow
import qualified MTL.Bench as MTL
import qualified Poly.Bench as Poly
import qualified ExtEff.Bench as ExtEff

wruns :: Int
wruns = 1

runs :: Int
runs = 10000

main :: IO ()
main = do
  putStrLn "*** Time benchmarks ***"
  defaultMain
    [
      bgroup "Countdown"
      [ bgroup "Put"
        [ bench "fused-effects" (nf Fused.countDownBench runs)
        , bench "mtl" (nf MTL.countDownBench runs)
        , bench "polysemy" (nf Poly.countDownBench runs)
        , bench "freer-simple" (nf Freer.countDownBench runs)
        , bench "extensible-effects" (nf ExtEff.countDownBench runs)
        , bench "shallow" (nf Shallow.countDownBench runs)
        ]
      , bgroup "Put+Exc"
        [ bench "fused-effects" (nf Fused.countDownExcBench runs)
        , bench "mtl" (nf MTL.countDownExcBench runs)
        , bench "polysemy" (nf Poly.countDownExcBench runs)
        , bench "freer-simple" (nf Freer.countDownExcBench runs)
        , bench "extensible-effects" (nf ExtEff.countDownExcBench runs)
        , bench "shallow" (nf Shallow.countDownExcBench runs)
        ]
      ]
    , bgroup "HTTP"
      [ bench "fused-effects" (nfAppIO Fused.httpBench runs)
      , bench "polysemy" (nfAppIO Poly.httpBench runs)
      , bench "extensible-effects" (nfAppIO ExtEff.httpBench runs)
      , bench "Deep embedding" (nfAppIO MTL.httpBench runs)
      , bench "Shallow embedding" (nfAppIO Shallow.httpBench runs)
      , bench "freer-simple" (nfAppIO Freer.httpBench runs)
      ]
    ]

  putStrLn "*** Space benchmarks (these will take some time to run) ***"
  Weigh.mainWith $ do
    Weigh.wgroup "Countdown" $ sequence_
      [ Weigh.func "fused-effects" Fused.countDownBench wruns
      , Weigh.func "mtl" MTL.countDownBench wruns
      , Weigh.func "polysemy" Poly.countDownBench wruns
      , Weigh.func "freer-simple" Freer.countDownBench wruns
      , Weigh.func "extensible-effects" ExtEff.countDownBench wruns
      , Weigh.func "shallow" Shallow.countDownBench wruns
      ]
    Weigh.wgroup "Countdown + exc" $ sequence_
      [ Weigh.func "fused-effects" Fused.countDownExcBench wruns
      , Weigh.func "mtl" MTL.countDownExcBench wruns
      , Weigh.func "polysemy" Poly.countDownExcBench wruns
      , Weigh.func "freer-simple" Freer.countDownExcBench wruns
      , Weigh.func "extensible-effects" ExtEff.countDownExcBench wruns
      , Weigh.func "shallow" Shallow.countDownExcBench wruns
      ]
