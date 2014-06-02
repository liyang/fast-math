{-# OPTIONS_GHC -O2 #-} 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This example performs a number of criterion benchmarks calculating various
-- Lp (Lebesgue) metrics.  The variable p is specified at the type level.
module Main
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Criterion.Config 
import Criterion.Main
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import Numeric.FastMath

-------------------------------------------------------------------------------
-- criterion tests

-- | size of each vector to test; must be divisible by 4
veclen = 10

-- | number of vectors in 2d tests
numvec = 1000

-- | criterion configuration parameters
critConfig = defaultConfig 
    { cfgPerformGC   = ljust True
    , cfgSamples     = ljust 100
    }

-------------------------------------------------------------------------------
-- main

main = do
    

    -----------------------------------
    -- initialize 1d vectors
    putStrLn "constructing 1d vectors "

    let dimL1 :: [Float] = 
            [ evalRand (getRandomR (-10000,10000)) (mkStdGen i) | i <- [1..numvec]]
        dimL2 :: [Float] = 
            [ evalRand (getRandomR (-10000,10000)) (mkStdGen i) | i <- [2..numvec+1]]

    let va = VG.fromList dimL1 :: VU.Vector Float
        vb = VG.fromList dimL2 :: VU.Vector Float

    deepseq va $ deepseq vb $ return ()

    -----------------------------------
    -- initialize 2d vectors

    putStrLn "constructing 2d vectors of vectors"

    let dimLL1 :: [[Float]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [1..numvec]]
        dimLL2 :: [[Float]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [2..numvec+1]]

    let vva = VG.fromList $ map VG.fromList dimLL1 
            :: V.Vector (VU.Vector Float)
        vvb = VG.fromList $ map VG.fromList dimLL2 
            :: V.Vector (VU.Vector Float)

    deepseq vva $ deepseq vvb $ return ()

    -----------------------------------
    -- tests

    putStrLn "starting criterion"

    defaultMainWith critConfig (return ())
        [ bench "1d" $ nf (l2distance va) vb
        , bench "2d" $ nf (distance_allToAll l2distance vva) vvb
        ]


-------------------------------------------------------------------------------
-- test functions 

-- | sums the distance between a point and every point in a vector in time O(n)
distance_oneToAll ::
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v2 f -> v1 (v2 f) -> f
distance_oneToAll !dist !v !vv =  go 0 (VG.length vv-1)
    where
        go !tot (-1) = tot
        go !tot !i = go tot' (i-1)
            where
                tot' = tot + dist v (vv `VG.unsafeIndex` i)

-- | sums the distance between every point in vv1 and every point in vv2 in time O(n^2)
distance_allToAll ::
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v1 (v2 f) -> v1 (v2 f) -> f
distance_allToAll !dist !vv1 !vv2 = go 0 (VG.length vv1-1)
    where
        go !tot (-1) = tot
        go !tot !i = go tot' (i-1)
            where
                tot' = tot + distance_oneToAll dist (vv1 `VG.unsafeIndex` i) vv2

-------------------------------------------------------------------------------
-- the L2 distance

l2distance :: VU.Vector Float -> VU.Vector Float -> Float
l2distance !v1 !v2 = (go 0 (VG.length v1-1))**(1/2)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1**2) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

