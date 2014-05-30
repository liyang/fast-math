-- | Compile-time optimisations for 'Float' and 'Double' that break IEEE-754
-- compatibility.
--
-- Namely, this otherwise empty module contains RULES that rewrite @x-x@,
-- @x*0@ and @0*x@ to @0@, which is incorrect (according to IEEE-754) when
-- @x@ is @NaN@.
--
-- At the time of writing, @base-4.3.1.0:GHC/Base.lhs@ erroneously includes
-- these rules for 'Float's, but not for 'Double's. This has been reported
-- as GHC bug #5178: <http://hackage.haskell.org/trac/ghc/ticket/5178>.

module Numeric.FastMath () where

import GHC.Exts

{-# RULES
"minusFloat x x"    forall x#. minusFloat#  x#   x#   = 0.0#
"timesFloat x 0.0"  forall x#. timesFloat#  x#   0.0# = 0.0#
"timesFloat 0.0 x"  forall x#. timesFloat#  0.0# x#   = 0.0#

"minusDouble x x"    forall x#. (-##) x#    x#    = 0.0##
"timesDouble 0.0 x"  forall x#. (*##) 0.0## x#    = 0.0##
"timesDouble x 0.0"  forall x#. (*##) x#    0.0## = 0.0##
  #-}

