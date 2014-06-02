-- | This module contains rules that break the way NaN is handled for "Float" 
-- and "Double" types.  Still, these rules should be safe in the vast majority of
-- applications.
--
-- Importing this module is similar to compiling with gcc's @-fno-signaling-nans@
-- and @-ffinite-math-only@.
--
module Numeric.FastMath.NaN
    where

import GHC.Exts

{-# RULES
"minusDouble x x"   forall x. (-##) x       x           = 0.0##

"timesDouble 0 x"   forall x. (*##) 0.0##   x           = 0.0##
"timesDouble x 0"   forall x. (*##) x       0.0##       = 0.0##

"divideDouble x 1"  forall x. (/##) x       1.0##       = x
"divideDouble x -1" forall x. (/##) x       (-1.0##)    = negateDouble# x
"divideDouble 0 x"  forall x. (/##) 0.0##   x           = 0.0##
  #-}

{-# RULES
"minusFloat x x"    forall x. minusFloat#  x    x       = 0.0#

"timesFloat x 0"    forall x. timesFloat#  x    0.0#    = 0.0#
"timesFloat 0 x"    forall x. timesFloat#  0.0# x       = 0.0#

"divideFloat x 1"   forall x. divideFloat# x       1.0#         = x
"divideFloat x -1"  forall x. divideFloat# x       (-1.0#)      = negateFloat# x
"divideFloat 0 x"   forall x. divideFloat# 0.0#    x            = 0.0#

  #-}

