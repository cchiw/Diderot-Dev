## Current status as of August 8, 2016

### Major bugs

  * https://github.com/Diderot-Language/examples dvr/dvr.diderot (Feb 14 email from glk subject
    "memory errors, possibly cause of crashing"): memory errors cause crashing.  Error messages
    suggest incorrect use of free().
    - fixed; the crashing bugs are gone.
  * bug054.diderot: return from inside a conditional in a function causes uncaught exception
    - fixed
  * bug047.diderot: don't know what's wrong; this is a program that used to work in the vis12 branch.
    - fixed
  * bug052.diderot: inside() doesn't work on 1D fields
    - fixed
  * bug053.diderot: differentiation doesn't work on 1D fields
    - fixed
  * bug043.diderot: generated broken C code (was: no output ints?)
    - fixed
  * bug045.diderot: missing determinant on tensor[2,2] and tensor[3,3]
    - fixed
  * bug050.diderot: tensor[3,3] output not working
    - fixed
  * bug055.diderot: conditional field assignment causes an uncaught exception
    - fixed (typechecker rejects program)

### Minor bugs

  * bug029.diderot missing code generation for printing matrices and higher-order tensors.
    - fixed
  * bug051.diderot: 10x increase in compile time changing from bspln3 to c4hexic (can use --disable-mid-check)
    - vis15 compiles the c4hexic version in 1.16 seconds without checking and in 29s with --check-all
      So it is faster than vis12, but still slow when internal checking is enabled.
  * bug028.diderot: missing forms of matrix multiplication; Charisee's work will address this issue
    - fixed
  * bug048.diderot: can't use one int and one real in a conditional expression
    - fixed
  * bug049.diderot: bogus types parsed as strand names create uncaught exceptions
    - fixed, but the error message could be improved
  * bug007.diderot: no codegen support for tensor slices; Charisee's work will address this issue
     - fixed
  * bug038.diderot: (re-opening an older bug) run-time should check on existence of field orientation meta-data before use
     - fixed
  * bug044.diderot: either implement or disallow "output bool"
     - fixed
  * bug040.diderot: "The avoidance of redundant Inside tests is not working."
    - still open

### Wish list (from high to low priority)

  * add `#version` directive.  `#version 1.0` will be old syntax (some merge of charisee and lamont's
    branches); `#version 2.0` will be the new syntax.  No directive will be interpreted as the
    same as `#version 1.0`.  Include a `--warn-deprecated` compiler flag to generate
    warnings about deprecated syntax.

  * add clerp, with the following semantics:
    ````
    clerp(A,B,U,x,V) == clamp(A,B,lerp(A,B,U,x,V))
    clerp(A,B,x) == clamp(A,B,lerp(A,B,x))
    ````
    - still open
  * allow a "continue" statement within the update method to go to the beginning of
    the next update iteration
    - fixed
  * print(): either implement for --target=pthread, or have compiler warn that it won't work
    - fixed
  * print(): should be possible during strand initialization.  Otherwise have to create an iteration
    counter initialized to zero, and print from inside update() only on first iteration
    - fixed
  * remove restriction on .diderot filename extension for Diderot program (or at least allow .ddro)
    - fixed
  * syntax errors from the compiler should just say "syntax error".  The "try" suggestions are
    almost always confusing, sometimes laughable (e.g. printf() not a valid function; should be
    print() but compiler suggests to try inserting "%=".  nope!).  For now, all suggestions should
    be removed, because on average they create more confusion. More carefully crafted suggestions
    can be added on a case by case basis.
    - fixed; we now just print "syntax error".  A better fix will require improvements to ml-antlr.
  * with "if (true) { foo } else { bar }", prune the dead code (in simplify?)
    - fixed
  * using "kern#N" syntax to assert C^N continuity
    - fixed
  * odd-support kernels
    - still open

### New and revised language features

  * add global `die` statement **DONE**

  * syntax changes:
    - `#version` declaration **DONE**
    - add keyword `initialize` to unamed blocks **DONE**
    - keyword `initially` replaced by `start` **DONE**
    - `grid(d)` replaced by `create_array` **DONE**
    - `image(<exp>)` replaced by `load_array(<exp>)` **DONE**
    - `load(<exp>)` replaced by `load_sequence(<exp>)` **DONE**
    - keyword `collection` replaced by `create_collection` **DONE**
    - support for orientation argument to `create_array`
    - `image` type becomes `array`
    - add a function to basis that returns the homogeneous coordinate orientation matrix of an array
    - add a global "finish" block to the language

  * `print`ing of `real` values should have sufficient precision to exactly recreate bit-level representation.  For example (without `--double`), `real x = sqrt(2); print(x, "\n");` prints `1.41421`, but need `1.4142135` to recreate the value.  If `printf` were being used, the conversions should be `%.9g` for `float` and `%.17g` for `double`.

  * be able to `print` Unicode characters

  * The expressivity of real literals should match that of floats and doubles in C. This currently breaks the principle
    that tensors should print as they can be parsed: `real x = 0.000001; print("x = ", x, "\n");` will print
    `x = 1e-06`, but `real x = 1e-06;` doesn't parse.  C also allows "1." for float or double 1.0; Diderot doesn't.
    This breaks the ability to copy C expressions from C code (or programs like Mathematica that can generate C expressions)
    into Diderot.  Of lesser importance is handling the "f", "F", "l", "L" type suffixes (which are meaningless for Diderot).

  * Augment the sphere() spatial query to permit specifying the strand set (active, stable, or all).  Right now, can only use "all".

  * overload `==` to also work on tensors

  * allow user-defined functions to be overloaded (as in GLSL)

  * generate an error or warning message if the 2^32 step count is exceeded (c.f. diderot/tests/new-tests/whystop.diderot)

  * add `+` as unary operator on integers, tensors, and fields

  * some way of creating long (>100 character) strings, split across multiple lines of source code, which is useful for carefully documenting input variables. Mimicking C's implicit (adjacency-based) string concatenation is probably most obvious form. or else the `+` operator

  * warnings when variables and built-in functions are shadowed: it is currently too easy to write program that are confusing in their intent or their behavior, because of shadowing.

  * ability to set input variables at compile time, especially input images. Currently have to either use no proxy and accept specialization to float-type samples, or have to copy the input image of interest to the particular filename in the Diderot source, or edit the Diderot source, because of (appropriate) specialization on array sizes.  In general, fixing some inputs at compile-time could produce performance benefits that users will want to explore.
