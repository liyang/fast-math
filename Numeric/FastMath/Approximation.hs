-- | This module contains rewrite rules that may change the lowest order bits
-- of a computation.  They take advantage of:
--
-- * distributivity
--
-- * repeated addition/multiplication
--
-- * exponentiation rules 
--
-- All of these RULES should be safe in the presence of `NaN` and `Infinity`
--
-- Importing this module is similar to compiling with gcc's
-- @-funsafe-math-operations@.
--
module Numeric.FastMath.Approximation
    where

import GHC.Exts
import Prelude

---------------------------------------
-- distributivity
--
-- NOTE: these rules are sufficient to capture the property
--
-- > x*y1+x*y2+x*y3 == x*(y1+y2+y3)
--
-- because they will be applied recursively during the optimization passes

{-# RULES

"double *,+ distribute A" forall x y1 y2. (x *## y1) +## (x *## y2) 
    = x *## (y1 +## y2)

"double *,+ distribute B" forall x y1 y2. (y1 *## x) +## (x *## y2) 
    = x *## (y1 +## y2)

"double *,+ distribute C" forall x y1 y2. (y1 *## x) +## (y2 *## x) 
    = x *## (y1 +## y2)

"double *,+ distribute D" forall x y1 y2. (x *## y1) +## (y2 *## x) 
    = x *## (y1 +## y2)



"double *,- distribute A" forall x y1 y2. (x *## y1) -## (x *## y2) 
    = x *## (y1 -## y2)

"double *,- distribute B" forall x y1 y2. (y1 *## x) -## (x *## y2) 
    = x *## (y1 -## y2)

"double *,- distribute C" forall x y1 y2. (y1 *## x) -## (y2 *## x) 
    = x *## (y1 -## y2)

"double *,- distribute D" forall x y1 y2. (x *## y1) -## (y2 *## x) 
    = x *## (y1 -## y2)



"double /,+ distribute" forall x y1 y2. (y1 /## x) +## (y2 /## x) 
    = (y1 +## y2) /## x

"double /,- distribute" forall x y1 y2. (y1 /## x) -## (y2 /## x) 
    = (y1 -## y2) /## x

  #-}



{-# RULES

"float *,+ distribute A" forall x y1 y2. (x `timesFloat#` y1) `plusFloat#` (x `timesFloat#` y2) 
    = x `timesFloat#` (y1 `plusFloat#` y2)

"float *,+ distribute B" forall x y1 y2. (y1 `timesFloat#` x) `plusFloat#` (x `timesFloat#` y2) 
    = x `timesFloat#` (y1 `plusFloat#` y2)

"float *,+ distribute C" forall x y1 y2. (y1 `timesFloat#` x) `plusFloat#` (y2 `timesFloat#` x) 
    = x `timesFloat#` (y1 `plusFloat#` y2)

"float *,+ distribute D" forall x y1 y2. (x `timesFloat#` y1) `plusFloat#` (y2 `timesFloat#` x) 
    = x `timesFloat#` (y1 `plusFloat#` y2)



"float *,- distribute A" forall x y1 y2. (x `timesFloat#` y1) `minusFloat#` (x `timesFloat#` y2) 
    = x `timesFloat#` (y1 `minusFloat#` y2)

"float *,- distribute B" forall x y1 y2. (y1 `timesFloat#` x) `minusFloat#` (x `timesFloat#` y2) 
    = x `timesFloat#` (y1 `minusFloat#` y2)

"float *,- distribute C" forall x y1 y2. (y1 `timesFloat#` x) `minusFloat#` (y2 `timesFloat#` x) 
    = x `timesFloat#` (y1 `minusFloat#` y2)

"float *,- distribute D" forall x y1 y2. (x `timesFloat#` y1) `minusFloat#` (y2 `timesFloat#` x) 
    = x `timesFloat#` (y1 `minusFloat#` y2)



"float /,+ distribute" forall x y1 y2. (y1 `timesFloat#` x) `plusFloat#` (y2 `timesFloat#` x) 
    = (y1 `plusFloat#` y2) `divideFloat#` x

"float /,- distribute" forall x y1 y2. (y1 `divideFloat#` x) `minusFloat#` (y2 `divideFloat#` x) 
    = (y1 `minusFloat#` y2) `divideFloat#` x

  #-}

---------------------------------------
-- fancy distributing
--
-- NOTE: I'm not yet sure if all of these are a great idea to have on by 
-- default due to stability issues...

{-# RULES

"double **,* distribute" forall x y1 y2. (y1 **## x) *## (y2 **## x) = (y1 *## y2) **## x

"double **,log distribute" forall x y. logDouble# (x **## y) = y *## (logDouble# x)

  #-}

---------------------------------------
-- Repeated addition
--
-- NOTE: It is important that these rules should fire after the distributivity
-- rules.  This ensures that
--
-- > x*x+x*y
--
-- gets simplified to
--
-- > x*(x+y)
--
-- rather than 
--
-- > x+x+x*y
--
{-# RULES 

"double mulToAdd 2" [0] forall x . x *## 2.0## = x +## x
"double mulToAdd 3" [0] forall x . x *## 3.0## = x +## x +## x
"double mulToAdd 4" [0] forall x . x *## 4.0## = x +## x +## x +## x

  #-}

{-# RULES

"float mulToAdd 2" [0] forall x . timesFloat# x 2.0# = plusFloat# x x
"float mulToAdd 3" [0] forall x . timesFloat# x 3.0# = plusFloat# x (plusFloat# x x)
"float mulToAdd 4" [0] forall x . timesFloat# x 4.0# = plusFloat# x (plusFloat# x (plusFloat# x x))

  #-}

---------------------------------------
-- left associate / commute

-- NOTE: phase controls are needed to prevent infinite loops when interacting 
-- with the repeated multiplication rules.
--
-- We should slightly prefer commuting rather than associating because it doesn't 
-- change the floating point results

{-# RULES

"double commute left *"   [~2] forall x1 x2 x3. (*##) x1 ((*##) x2 x3) = (*##) ((*##) x2 x3) x1
"double associate left *" [~1] forall x1 x2 x3. (*##) x1 ((*##) x2 x3) = (*##) ((*##) x1 x2) x3

"double commute left +"   [~2] forall x1 x2 x3. (+##) x1 ((+##) x2 x3) = (+##) ((+##) x2 x3) x1
"double associate left +" [~1] forall x1 x2 x3. (+##) x1 ((+##) x2 x3) = (+##) ((+##) x1 x2) x3

  #-}

{-# RULES

"float commute left *"   [~2] forall x1 x2 x3. timesFloat# x1 (timesFloat# x2 x3) = timesFloat# (timesFloat# x2 x3) x1
"float associate left *" [~1] forall x1 x2 x3. timesFloat# x1 (timesFloat# x2 x3) = timesFloat# (timesFloat# x1 x2) x3

"float commute left +"   [~2] forall x1 x2 x3. plusFloat# x1 (plusFloat# x2 x3) = plusFloat# (plusFloat# x2 x3) x1
"float associate left +" [~1] forall x1 x2 x3. plusFloat# x1 (plusFloat# x2 x3) = plusFloat# (plusFloat# x1 x2) x3

  #-}

---------------------------------------
-- Repeated multiplication

-- FIXME: I can't get thise rules to work for more than 4 repeats without
-- causing an infinite loop in the simplifier

{-# RULES

"double repmul 4" [1] forall x . ((x *## x) *## x) *## x 
    = let xx = (x *## x) in (xx *## xx)

  #-}

-- "double repmul 5" forall x . x *## x *## x *## x *## x 
--     = let xx = x *## x in xx *## xx *## x
-- 
-- "double repmul 6" forall x . x *## x *## x *## x *## x *## x
--     = let xx = x *## x in xx *## xx *## xx
-- 
-- "double repmul 7" forall x . x *## x *## x *## x *## x *## x *## x
--     = let xx = x *## x in xx *## xx *## xx *## x
-- 
-- "double repmul 8" forall x . x *## x *## x *## x *## x *## x *## x *## x 
--     = let xxx = (let xx = x *## x in xx *## xx) in xxx *## xxx

{-# RULES

"double repmul 4" forall x . timesFloat# x (timesFloat# x (timesFloat# x x))
    = let xx = timesFloat# x x in timesFloat# xx xx

  #-}

-- "double repmul 5" forall x . timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x x)))
--     = let xx = timesFloat# x x in timesFloat# x (timesFloat# xx xx)
-- 
-- "double repmul 6" forall x . timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x x))))
--     = let xx = timesFloat# x x in timesFloat# xx (timesFloat# xx xx)
-- 
-- "double repmul 7" forall x . timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x x)))))
--     = let xx = timesFloat# x x in timesFloat# x (timesFloat# xx (timesFloat# xx xx))
-- 
-- "double repmul 8" forall x . timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x (timesFloat# x x))))))
--     = let xxx = (let xx = timesFloat# x x in timesFloat# xx xx) in timesFloat# xxx xxx


---------------------------------------
-- Exponentiation 

{-# RULES 
"double **0" forall x . x **## 0.0## = 1.0##
"double **1" forall x . x **## 1.0## = x
"double **2" forall x . x **## 2.0## = x *## x
"double **3" forall x . x **## 3.0## = x *## x *## x
"double **4" forall x . x **## 4.0## = let xx = x *## x in xx *## xx
"double **8" forall x . x **## 8.0## = let xxx = (let xx = x *## x in xx *## xx) in xxx *## xxx

"double **(1/2)" forall x## . x## **## 0.500## = sqrtDouble# x##
"double **(1/4)" forall x## . x## **## 0.250## = sqrtDouble# (sqrtDouble# x##)
"double **(1/8)" forall x## . x## **## 0.125## = sqrtDouble# (sqrtDouble# (sqrtDouble# x##))
  #-}

{-# RULES
"float **0" forall x# . powerFloat# x# 0.0# = 1.0#
"float **1" forall x# . powerFloat# x# 1.0# = x#
"float **2" forall x# . powerFloat# x# 2.0# = timesFloat# x# x#
"float **3" forall x# . powerFloat# x# 3.0# = timesFloat# (timesFloat# x# x#) x#
"float **4" forall x# . powerFloat# x# 4.0# = let xx# = (timesFloat# x# x#) in timesFloat# xx# xx#
"float **8" forall x# . powerFloat# x# 8.0# = let xxx# = (let xx# = (timesFloat# x# x#) in timesFloat# xx# xx#) in timesFloat# xxx# xxx#

"float **(1/2)" forall x# . powerFloat# x# 0.500# = sqrtFloat# x#
"float **(1/4)" forall x# . powerFloat# x# 0.250# = sqrtFloat# (sqrtFloat# x#)
"float **(1/8)" forall x# . powerFloat# x# 0.125# = sqrtFloat# (sqrtFloat# (sqrtFloat# x#))
  #-}

