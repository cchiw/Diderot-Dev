# FIXME issues
Organized by source directory.  We use the following command to find the FIXMEs:

````
bbfind --gui --name-pattern "*.in" --name-pattern "*.sml" FIXME .
````

High priority items are marked with a "&#x1F525;" symbol.

## Compiler issues

### `ast`

  * be consistent about terminology for the various kinds of types (concrete, value, etc.)

### `basis`

  * additional basis operations:
    - add clerp3 and clerp5 operators (`basis-vars.sml`)
    - sum reduction on arbitrary tensors. (`basis-vars.sml`)
    - variance reduction (`basis-vars.sml`)

### `cfg-ir`

  * add additional checking (`check-ir-fn.sml`)
    - redundant bindings in the presence of loops
    - check the types of `SEQ` elements
    - check `EINAPP` bodies
    - check application of user-defined functions
    - check `MAPREDUCE` nodes
    - fix check for rebinding in `FOREACH` nodes (currently disabled)

  * handle `EINAPP` nodes in `toString` function (`expr-fn.sml`)

  * handle fused `MAPREDUCE` in global-variable promotion (`promote-fn.sml`)

  * avoid redundancy of setting bindings for variables (`translate-fn.sml` and `census-fn.sml`)

  * delete `APPLY` when function is pure and the result is unused (`unused-elim-fn.sml`)

  * delete `MAPREDUCE` when body is pure and the result is unused (`unused-elim-fn.sml`)

### `codegen`

  * precedence checking for `mkApply` (`clang.sml`)

  * printing "const" types (`print-as-c.sml`)

### `common`

  * determine kernel continuity from kernel definitions (`kernel.sml`)

### `cxx-util`

  * generate error-handling code that puts the error message in the error buffer and returns
    and error status (`gen-outputs-util.sml`)

  * various issues when synthesizing types and operations (`gen-tys-and-ops.sml`)
    - tuple types are not supported
    - &#x1F525; some dynamic sequence types are not supported (e.g., fixed-size sequences)
    - generating printing code for tuples

  * printing types needs to be overhauled to correctly handle C++ type syntax (`print-as-cxx.sml`)

  * printing support for classes that include protected or private members (`print-as-cxx.sml`)

  * many issues in translating Tree IR to C++ (`tree-to-cxx.sml`)
    - liveness analysis to enable *in situ* dynamic sequence operations
    - add `vload_aligned` and `vpack_aligned` operations
    - refactor common code out of `trAssign` and `trDecl` functions

  * handle tuple types in `trQType` and `trType` (`type-to-cxx.sml`)

### `driver`

No issues.

### `ein`

  * use the `BorderCtl.ctl` type instead of defining a new type (`ein.sml`)

### `fields`

No issues.

### `global-env`

No issues.

### `high-ir`

No issues.

### `high-opt`

  * improve efficiency of doRHS by doing one pass of substitutions (`normalize.sml`)

### `high-to-mid`

  * Expand integer powers into multiplications (`high-to-mid.sml`)

### `inputs`

No issues.

### `low-ir`

No issues.

### `low-opt`

No issues.

### `low-to-tree`

  * many issues in translating Low IR to Tree IR (`low-to-tree.sml`)
    - add support for output globals in `mkGlobalVar`
    - &#x1F525; `RealToInt` translation when the argument has multiple pieces

### `mid-ir`

No issues.

### `mid-opt`

No issues.

### `mid-to-low`

  * Support IR operators `Clamp`, `MapClamp`, and `Lerp` on higher-order tensor types
    (`mid-to-low.sml`).

  * Generate vector operations instead of scalar operations in more places (`ein-to-low.sml`,
    `ein-to-scalar.sml`, and `ein-to-vector.sml`)

### `mlton`

No issues.

### `nrrd`

No issues.

### `options`

No issues.

### `parse-tree`

No issues.

### `parser`

No issues.

### `simple`

  * Invariant checking for Simple AST representation (`check-simple.sml`)

  * Pretty-printing for fused map-reduce (`simple-pp.sml`)

### `simple-opt`

  * Support fusion of reductions (`map-reduce-opt.sml`)

### `simplify`

  * Add a logical negate operation on AST expressions to support better if-then-else
    translations (`simplify.sml`)

  * &#x1F525; Handle `AST.E_ParallelMap` expressions (`simplify.sml`)

  * Support for `variance` reductions over strand sets and sequences (`simplify.sml`)

### `target-cpu`

  * switch to building an AST once CLang supports `const` functions (`gen-strand.sml`)

  * &#x1F525; handle create iterations over sequences (`gen-world.sml`)

  * &#x1F525; residual constants during code generation? (`gen.sml`)

  * &#x1F525; support for parallel target (`gen.sml` and `fragments`)

### `translate`

  * Support for `E_Tuple` and `E_Project` (`translate.sml`)

  * Check possible issues with global initialization and conditionals (`translate.sml`)

  * Check possible issues with strand methods that do not reach the exit node (`translate.sml`)

### `tree-ir`

  * Many issues in checking the Tree IR (`check-tree.sml`)
    - Parameterize the `check` function by the target's vector layout rules and
      check for valid layouts
    - Finish checking for `S_MAssign` statements
    - Check body of map-reduce
    - Checking for `S_LoadNrrd` statements
    - Check user-defined functions

### `tree-opt`

No issues.

### `typechecker`

  * Evaluation of constant tensor slice (`check-const.sml`)

  * Check optional frame argument to `create_array` (once feature is enabled)

  * Many issues in typechecking expressions (`check-expr.sml`)
    - improved overload resolution
    - resulting differentiation for inner, outer, and colon products
    - check index against shape of tensor for subscripting/slicing
    - check for sequences of non-concrete types (*e.g.*, sequences of fields)
    - non-literal constant string expressions

  * Handle command-line definitions of constants (`check-globals.sml`)

  * Allow strands without outputs, once we have global outputs (`check-strand.sml`)

  * Check for globals and state variables that are not initialized in
    their respective initialization sections (`check-var-uses.sml`)

  * don't report the `pos` state variable as unused when there are spatial queries

  * Check for situation where `initially` method has call to `stabilize` (`check-var-uses.sml`)

  * Constants that are solely used to initialize other constants are reported
    unused (`check-var-uses.sml`)

  * Unification of differentiation bounds (`unify.sml`)

## Runtime system issues

### `common`

  * figure out a mechanism to pass warnings back up the call chain, instead of printing
    them from library code (`world.cxx`)

### `include/diderot`

  * &#x1F525; We need to check all of the nrrd-file metadata that the compiler uses when loading
    an image file (`image-inst.hxx`)

  * For programs that do not use `new` or `die`, we can allocate less space for the
    KD-tree partitions (`kdtree-inst.hxx`)

# Questions

#### `ein`

  * QUESTION: should the `int` be `IntInf.int`?  (`ein.sml`)
    - Not sure CC

  * QUESTION: why is this `index_kind` list and not `index_id` list? (`ein.sml`)
    - They are both ints, but one is a variable index
      and the other binds the ranges of the variable indices.
