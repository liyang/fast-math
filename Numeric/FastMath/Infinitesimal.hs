-- | Also rewrite @0/x@ to @+0@, which should really be @-0@ for negative
-- @x@, and @NaN@ when @x@ = 0.

module Numeric.FastMath.Infinitesimal () where

import GHC.Exts

{-# RULES
"divideFloat 0 x"   forall x. divideFloat#  0.0#    x   = 0.0#
"divideDouble 0 x"  forall x. (/##)         0.0##   x   = 0.0##
  #-}

