## Collected syntax of Diderot


:---------------|:--:|:-----------
_Program_ | ::= | { _GlobalDcl_ } [ _Block_ ] _StrandDcl_ _InitialStrands_ [ _GlobalInitially_ ] [ _GlobalUpdate_ ]
| | | |
_GlobalDcl_ | ::= | _InputDcl_
  | \| | _ConstDcl_
  | \| | _GlobalVarDcl_
  | \| | _FunctionDcl_
| | | |
_InputDcl_ | ::= | **input** _ValueType_ Id [ `(` String `)` ] [ `=` _ConstExpr_ ] `;`
| | | |
_ConstDcl_ | ::= | **const** _ConcreteType_ Id [ `=` _ConstExpr_ ] `;`
| | | |
_GlobalVarDcl_ | ::= | _Type_ Id [ `=` _Expression_ ] `;`
| | | |
_FunctionDcl_ | ::= | **function** _Type_ Id `(` _Parameters_ `)` `=` _Expression_ `;`
  | \| | **function** _Type_ Id `(` _Parameters_ `)` _Block_
| | | |
_Parameters_ | ::= | [ _Parameter_ { `,` _Parameter_ } ]
| | | |
_Parameter_ | ::= | _Type_ Id
| | | |
_StrandDcl_ | ::= | **strand** Id _Parameters_ `{` { _StateVarDcl_ } [ _Block_ ] { _MethodDcl_ } `}`
| | | |
_StateVarDcl_ | ::= | _ConcreteType_ Id [ `=` _Expression_ ] `;`
| | | |
_MethodDcl_ | ::= | _MethodName_ _Block_
| | | |
_MethodName_ | ::= | **initially** \| **update** \| **stabilize**
| | | |
_InitialStrands_ | ::= | **collection** _Comprehension_
  | \| | **grid** `(` _ConstExpr_ `)` _Comprehension_
| | | |
_GlobalInitially_ | ::= | **initially** _Block_
| | | |
_GlobalUpdate_ | ::= | **update** _Block_
| | | |
_Block_ | ::= | `{` { _Statement_ } `}`
| | | |
_Statement_ | ::= | _Block_
  | \| | **if** `(` _Expression_ `)` _Block_ { _ElseIf_ } [ **else** _Block_ ]
  | \| | **foreach** `(` _Iterator_ `)` _Block_
  | \| | **print** `(` _Expression_ { `,` _Expression_ } `)` `;`
  | \| | **new** Id `(` _Arguments_ `)` `;`
  | \| | **stabilize** `;`
  | \| | **die** `;`
  | \| | **continue** `;`
  | \| | **return** _Expression_ `;`
  | \| | _Type_ Id `=` _Expression_ `;`
  | \| | Id _AssignOp_ _Expression_ `;`
| | | |
_ElseIf_ | ::= | **else** **if** `(` _Expression_ `)` _Block_
| | | |
_AssignOp_ | ::= | `=` \| `+=` \| `-=` \| `*=` \| `/=` \| `%=`
| | | |
_Type_ | ::= | **field** `#` _Dimension_ `(` _ConstExpr_ `)` _Shape_
  | \| | **kernel** `#` _Dimension_
  | \| | _ValueType_
| | | |
_Dimension_ | ::= | Int
| | | |
_ValueType_ | ::= | **image** `(` _ConstExpr_ `)` _Shape_
  | \| | _ConcreteType_
| | | |
_ConcreteType_ | ::= | _PrimitiveType_ { _SequenceDims_ }
| | | |
_SequenceDims_ | ::= |
  | \| | `[` `]`
  | \| | `[` _ConstExpr_ `]`
| | | |
_PrimitiveType_ | ::= | **tensor** _Shape_
  | \| | **vec2**
  | \| | **vec3**
  | \| | **vec4**
  | \| | **mat2**
  | \| | **mat3**
  | \| | **mat4**
  | \| | **bool**
  | \| | **int**
  | \| | **real**
  | \| | **string**
| | | |
_Shape_ | ::= | `[` [ _ConstExpr_ { `,` _ConstExpr_ } ] `]`
| | | |
_Comprehension_ | ::= | `{` _Expression_ `|` _Iterator_ { `,` _Iterator_ } `}`
| | | |
_Iterator_ | ::= | Id **in** _Expression_
| | | |
_Expression_ | ::= | _RangeExpr_ [ `?` _Expression_ `:` _Expression_ ]
| | | |
_RangeExpr_ | ::= | _OrExpr_ `..` _OrExpr_
| | | |
_OrExpr_ | ::= | _AndExpr_ { `||` _AndExpr_ }
| | | |
_AndExpr_ | ::= | _CompareExpr_ { `&&` _CompareExpr_ }
| | | |
_CompareExpr_ | ::= | _AddExpr_ { _CompareOp_ _AddExpr_ }
| | | |
_CompareOp_ | ::= |  `<` \| `<=` \| `==` \| `!=` \| `>=` \| `>`
| | | |
_AddExpr_ | ::= | _MultiplyExpr_ { _AddOp_ _MultiplyExpr_ }
| | | |
_AddOp_ | ::= | `+` \| `-` \| `@`
| | | |
_MultiplyExpr_ | ::= | _PrefixExpr_ { _MultiplyOp_ _PrefixExpr_ }
| | | |
_MultiplyOp_ | ::= | `*` \| `/` \| `%` \| `⊛` \| `•` \| `×` \| `⊗` \| `:`
| | | |
_PrefixExpr_ | ::= | _PowerExpr_
  | \| | _PrefixOp_ _PrefixExpr_
| | | |
_PrefixOp_ | ::= | `-` \| `!`
| | | |
_PowerExpr_ | ::= | _SuffixExpr_ { `^` _SuffixExpr_ }
| | | |
_SuffixExpr_ | ::= | _DiffExpr_ { _Suffix_ }
  | \| | **real** `(` _Expression_ `)`
  | \| | **load** `(` _ConstExpr_ `)`
  | \| | **image** `(` _ConstExpr_ `)`
| | | |
_Suffix_ | ::= | `(` _Arguments_ `)`
  | \| | `[` _Indices_ `]`
  | \| | `.` Id
| | | |
_Indices_ | ::= | _IndexExpr_ [ `,` _IndexExpr_ ]
| | | |
_IndexExpr_ | ::= | _Expression_
  | \| | `:`
| | | |
_DiffExpr_ | ::= | _AtomicExpr_
  | \| | _DiffOp_ _DiffExpr_
| | | |
_DiffOp_ | ::= | `∇` \| `∇⊗` \| `∇×` \| `∇•`
| | | |
_AtomicExpr_ | ::= | Id
  |  \| | Id `#` _Dimension_
  | \| | Int
  | \| | Real
  | \| | String
  | \| | **true**
  | \| | **false**
  | \| | **identity** `[` _ConstExpr_ `]`
  | \| | **zeros** [ _Shape_ ]
  | \| | **nan** [ _Shape_ ]
  | \| | `{` _Expression_ `|` _Iterator_ `}`
  | \| | `(` _Expression_ { `,` _Expression_ } `)`
  | \| | `{` `}`
  | \| | `{` _Expression_ { `,` _Expression_ } `}`
  | \| | `[` _Expression_ { `,` _Expression_ } `]`
  | \| | `|` _Expression_ `|`
  | \| | `(` _Expression_ `)`
| | | |
_Arguments_ | ::= | [ _Expression_ { `,` _Expression_ } ]
| | | |
_ConstExpr_ | ::= | _Expression_


### Syntactic restrictions

* Constant expressions (_ConstExpr_) are restricted to have a concrete type (_ConcreteType_)
  and be can be evaluated statically.

* A sequence type can have at most one dynamic dimension (*e.g.*, `int [][3]` or
  `int [3][]`, but not `int[][]`).

* When indexing a tensor or field (e.g., `e1[e2]`), the index expression must be a constant
  expression.

* Conditional expressions must have value type.

* Assignment to global variables is not permitted inside strands.

* Strands must have an `update` method

* The **stabilize** statement is only permitted inside a strand's **initially** or **update**
  method (where it affects the calling strand) or in a global **initially** or **update**
  block (where it stabilizes all active strands).

* The **die** statement is only permitted inside a strand's **initially** or **update**
  method.

### Lexical issues

* Comments are as in C++ (either `//` to end of line or enclosed in `/*` `*/`)

### Questions

* What about tuples?  Can we have sequences of tuples?  Tuples of sequences?  Fields inside
  tuples?
