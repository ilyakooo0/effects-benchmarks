{-# LANGUAGE FlexibleContexts, GADTs, CPP #-}

module Main where

import Gauge (bench, bgroup, nf, nfAppIO)
import Gauge.Main (defaultMain)
import qualified Weigh

import qualified Fused.Stateful as Fused
import qualified Freer.Stateful as Freer
import qualified Shallow.Stateful as Shallow
import qualified MTL.Stateful as MTL
import qualified Poly.Stateful as Poly
import qualified ExtEff.Stateful as ExtEff

import qualified Fused.StatefulExcept as Fused
import qualified Freer.StatefulExcept as Freer
import qualified Shallow.StatefulExcept as Shallow
import qualified MTL.StatefulExcept as MTL
import qualified Poly.StatefulExcept as Poly
import qualified ExtEff.StatefulExcept as ExtEff

import qualified Fused.HTTP as Fused
import qualified Freer.HTTP as Freer
import qualified MTL.HTTP as MTL
import qualified Shallow.HTTP as Shallow
import qualified Poly.HTTP as Poly
import qualified ExtEff.HTTP as ExtEff

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
        [ bench "fused-effects" (nf Fused.countDownPut runs)
        , bench "mtl" (nf MTL.countDownPut runs)
        , bench "polysemy" (nf Poly.countDownPut runs)
        , bench "freer-simple" (nf Freer.countDownPut runs)
        , bench "extensible-effects" (nf ExtEff.countDownPut runs)
        , bench "shallow" (nf Shallow.countDownPut runs)
        ]
      , bgroup "Put+Exc"
        [ bench "fused-effects" (nf Fused.countDownExc runs)
        , bench "mtl" (nf MTL.countDownExc runs)
        , bench "polysemy" (nf Poly.countDownExc runs)
        , bench "freer-simple" (nf Freer.countDownExc runs)
        , bench "extensible-effects" (nf ExtEff.countDownExc runs)
        , bench "shallow" (nf Shallow.countDownExc runs)
        ]
      ]
    , bgroup "HTTP"
      [ bench "fused-effects" (nfAppIO Fused.doHTTP runs)
      , bench "polysemy" (nfAppIO Poly.doHTTP runs)
      , bench "extensible-effects" (nfAppIO ExtEff.doHTTP runs)
      , bench "Deep embedding" (nfAppIO MTL.doHTTP runs)
      , bench "Shallow embedding" (nfAppIO Shallow.doHTTP runs)
      , bench "freer-simple" (nfAppIO Freer.doHTTP runs)
      ]
    ]

  putStrLn "*** Space benchmarks (these will take some time to run) ***"
  Weigh.mainWith $ do
    Weigh.wgroup "Countdown" $ sequence_
      [ Weigh.func "fused-effects" Fused.countDownPut wruns
      , Weigh.func "mtl" MTL.countDownPut wruns
      , Weigh.func "polysemy" Poly.countDownPut wruns
      , Weigh.func "freer-simple" Freer.countDownPut wruns
      , Weigh.func "extensible-effects" ExtEff.countDownPut wruns
      , Weigh.func "shallow" Shallow.countDownPut wruns
      ]
    Weigh.wgroup "Countdown + exc" $ sequence_
      [ Weigh.func "fused-effects" Fused.countDownExc wruns
      , Weigh.func "mtl" MTL.countDownExc wruns
      , Weigh.func "polysemy" Poly.countDownExc wruns
      , Weigh.func "freer-simple" Freer.countDownExc wruns
      , Weigh.func "extensible-effects" ExtEff.countDownExc wruns
      , Weigh.func "shallow" Shallow.countDownExc wruns
      ]
