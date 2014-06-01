-- | This module contains rules that break the way NaN is handled for "Float" 
-- and "Double" types.  Still, these rules should be safe in the vast majority of
-- applications.
module Numeric.FastMath.NaN
    where

import GHC.Exts

{-# RULES
"minusFloat x x"    forall x. minusFloat#  x    x       = 0.0#
"timesFloat x 0"    forall x. timesFloat#  x    0.0#    = 0.0#
"timesFloat 0 x"    forall x. timesFloat#  0.0# x       = 0.0#

"minusDouble x x"   forall x. (-##) x       x       = 0.0##
"timesDouble 0 x"   forall x. (*##) 0.0##   x       = 0.0##
"timesDouble x 0"   forall x. (*##) x       0.0##   = 0.0##
  #-}

