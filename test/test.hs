{-# OPTIONS_GHC -ddump-rule-firings -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -O2 -fsimpl-tick-factor=100 #-}

module Main
    where

import System.IO
import Numeric.FastMath

main = do
    
    -----------------------------------
    -- define constants in a way that prevents constant folding

    str1 <- getLine
    str2 <- getLine
    str3 <- getLine
    str4 <- getLine

    let f1 = read str1 :: Float
        f2 = read str2 :: Float
        f3 = read str3 :: Float
        f4 = read str4 :: Float
        d1 = read str1 :: Double
        d2 = read str2 :: Double
        d3 = read str3 :: Double
        d4 = read str4 :: Double

--     let f1=2::Float
--         f2=3::Float
--         f3=4::Float
--         d1=2::Double
--         d2=3::Double
--         d3=4::Double

    -----------------------------------
    putStrLn "distribute */+"

    let dist = d1*d2+d1*d3
        {-# NOINLINE dist #-}
    print dist

    let test1 :: Double -> Double
        test1 d = d*10 + d*20
        {-# NOINLINE test1 #-}
    print $ test1 d1

    let test2 :: Double -> Double -> Double
        test2 d1 d2 = d1*10 + d1*d1 + d1*20 
        {-# NOINLINE test2 #-}
    print $ test2 d1 d2

    let dist3 = d1*d2 + (d3 + 5)*d1 + d1*32
        {-# NOINLINE dist3 #-}
    print dist3

    -----------------------------------
    putStrLn "distribute more"

    let dist4 = d1*d2-d1*d3+d4*d1-d1*d1
        {-# NOINLINE dist4 #-}
    print dist4

    -----------------------------------
    putStrLn "mulToAdd"

    let test3 = d1*3 + d1*d2 + d1*d3  -- this FAILS
        {-# NOINLINE test3 #-}
    print test3

    -----------------------------------
    putStrLn "repadd"

    -----------------------------------
    putStrLn "repmul"

    print $ d1*d1*d1*d1

    let test5 = d1*(d1*(d1*d1)*d1*d1*(d1*d1)*d1)*d1
        {-# NOINLINE test5 #-}
    print test5

{-
    -----------------------------------
    putStrLn "**"
    
    print $ f1**(1/8)
    print $ f1**8
    
    print $ d1**(1/8)
    print $ d1**8

  -}  
    -----------------------------------
    putStrLn "I did nothing!"
