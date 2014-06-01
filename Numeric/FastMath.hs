-- | This module loads all rewrite rules.  Unless you know that some rules
-- will be unsafe for your application, this is the module you should load.
--
-- The best way to figure out what optimizations these modules do is by 
-- looking at the source code.  RULES pragmas are surprisingly readable.

module Numeric.FastMath
    ( module Numeric.FastMath.Approximation
    , module Numeric.FastMath.NaN
    , module Numeric.FastMath.Infinitesimal
    )
    where

import Numeric.FastMath.Approximation ()
import Numeric.FastMath.NaN ()
import Numeric.FastMath.Infinitesimal ()
