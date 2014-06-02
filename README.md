# What is fast-math?

This package enables a number of "unsafe" floating point optimizations for GHC.  For example, the distributive law:

```
x*y + x*z == z*(y+z)
```

does not hold for `Float` or `Double` types.  The lowest order bits may be different due to rounding errors.Therefore, GHC (and most compilers for any language) will not perform this optimization by default.   Instead, most compilers support special flags that enable these unsafe optimizations.  See for example the [-ffast-math flag in the gcc documentation](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html) and the [gcc wiki page FloatingPointMath](https://gcc.gnu.org/wiki/FloatingPointMath).  GHC, however, has [no built in flags for these optimizations](http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/flag-reference.html).  But that's okay.  GHC's `RULES` pragmas are sufficiently powerful to achieve most of the performance benefits of `-ffast-math`. This package provides those `RULES` pragmas.  

### Enabling the optimizations

To enable these optimizations in your code, simply add the following line to the top of your source files:

```
import Numeric.FastMath
```

For most users, this is all you need to do.  But some advanced code will depend on specific properties of the IEEE 754 standard, and so will want to enable only some of the optimizations.  The module structure of `fast-math` makes this easy.  Every module corresponds to a certain family of optimizations.  Importing that module enables only those optimizations.  For example, to enable optimizations that are unsafe only in the presence of `NaN` values, we would add the line:

```
import Numeric.FastMath.NaN
```

### How complete are the optimizations?

There are still some optimizations that gcc's `-ffast-math` flag supports that this library doesn't support.This is mostly due to limitations in the way `RULES` pragmas work.  For example, [constant folding](https://en.wikipedia.org/wiki/Constant_folding) cannot be implemented with `RULES`.  Instead, GHC implements this optimization as a special case in the file [compiler/prelude/PrelRules.lhs](https://github.com/ghc/ghc/blob/master/compiler/prelude/PrelRules.lhs).

Consider the code:

```
test1 :: Double -> Double
test1 d = d*10 + d*20
```

With the `fast-math` library, GHC factors out `d`, then folds the constants, producing the core:

```
test1 :: Double -> Double
test1 = \ (d :: Double) ->
    case d of _ { D# x -> D# (*## x 30.0) }
```

But if we make the code just a little more complicated:

```
test2 d1 d2 = d1*10 + d1*d2 + d1*20 
```

Then GHC distributes successfuly, but can't figure out how to fold the constants.  It produces the core:

```
test2 :: Double -> Double -> Double
test2 = \ (d1 :: Double) (d2 :: Double) ->
    case d1 of _ { D# x ->
    case d2 of _ { D# y ->
    D# (*## x (+## (+## 10.0 y) 20.0))
    }
    }
```

If we change the code so that the constants appear next to each other:

```
test3 d1 d2 = d1*d2 + d1*10 + d1*20 
```

then GHC successfully combines the constants into the core:

```
test3 :: Double -> Double -> Double
test3 = \ (d1 :: Double) (d2 :: Double) ->
    case d1 of _ { D# x ->
    case d2 of _ { D# y ->
    D# (*## x (+## y 30.0))
    }
    }
```

We could fix this problem if the `RULES` pragmas could identify which terms are constants and which are variables.  This would let us commute/associate the constants to the left of all computations, then GHC's standard constant folding mechanism would work successfully.

**The best way to check what optimizations are actually supported is to look at the source code.**  `RULES` pragmas are surprisingly readable.

### How does this interact with LLVM?

The LLVM backend can perform a number of these optimizations for us as well if we pass it the right flags.  It does not perform all of them, however.  (Possibly GHC's optimization passes remove the opportunity?)  In any event, executables from the built-in code generator and llvm generator will both see speed improvements.

### How does this interact with SIMD instructions?

Currently, there is no support for GHC 7.8's SIMD instructions.  This will hopefully appear in a future release.

### Installation

This package is [available on hackage](http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/flag-reference.html), and can be easily installed with:

```
cabal update
cabal install fast-math
```

