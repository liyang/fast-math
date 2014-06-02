-- | IEEE 754 math makes a distrinction between -0.0 and +0.0.  This module
-- contains RULES that ignore this distinction.  
--
-- Importing this module is similar to compiling with gcc's
-- @-fno-signed-zeros@.

module Numeric.FastMath.SignedZeros () where

import GHC.Exts

{-# RULES

"minusDouble 0 x"   forall x. (-##)         0.0##   x   = negateDouble# x
"divideDouble 0 x"  forall x. (/##)         0.0##   x   = 0.0##
  #-}

{-# RULES

"minusFloat 0 x"    forall x. minusFloat#   0.0#    x   = negateFloat# x
"divideFloat 0 x"   forall x. divideFloat#  0.0#    x   = 0.0#

  #-}


