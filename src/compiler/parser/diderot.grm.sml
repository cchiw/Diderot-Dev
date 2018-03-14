structure DiderotTokens =
  struct
    datatype token
      = KW_bool
      | KW_const
      | KW_continue
      | KW_create_array
      | KW_create_collection
      | KW_die
      | KW_else
      | KW_false
      | KW_field
      | KW_foreach
      | KW_function
      | KW_identity
      | KW_if
      | KW_image
      | KW_in
      | KW_initialize
      | KW_input
      | KW_int
      | KW_kernel
      | KW_load_image
      | KW_load_sequence
      | KW_mat2
      | KW_mat3
      | KW_mat4
      | KW_nan
      | KW_new
      | KW_output
      | KW_print
      | KW_real
      | KW_return
      | KW_stabilize
      | KW_start
      | KW_strand
      | KW_string
      | KW_tensor
      | KW_true
      | KW_update
      | KW_vec2
      | KW_vec3
      | KW_vec4
      | KW_zeros
      | KW_OField
      | KW_fem
      | KW_mesh
      | KW_element
      | KW_fnspace
      | OP_eq
      | OP_pluseq
      | OP_minuseq
      | OP_stareq
      | OP_slasheq
      | OP_modeq
      | OP_orelse
      | OP_andalso
      | OP_lt
      | OP_lte
      | OP_eqeq
      | OP_neq
      | OP_gte
      | OP_gt
      | OP_plus
      | OP_minus
      | OP_star
      | OP_convolve
      | OP_dot
      | OP_cross
      | OP_outer
      | OP_slash
      | OP_mod
      | OP_exp
      | OP_comp
      | OP_at
      | OP_D
      | OP_Dotimes
      | OP_curl
      | OP_Ddot
      | LP
      | RP
      | LB
      | RB
      | LCB
      | RCB
      | COMMA
      | SEMI
      | COLON
      | HASH
      | BANG
      | BAR
      | DOT
      | DOTDOT
      | VERSION of int list
      | ID of Atom.atom
      | INT of IntLit.t
      | REAL of RealLit.t
      | STRING of string
      | EOF
    val allToks = [
            KW_bool, KW_const, KW_continue, KW_create_array, KW_create_collection, KW_die, KW_else, KW_false, KW_field, KW_foreach, KW_function, KW_identity, KW_if, KW_image, KW_in, KW_initialize, KW_input, KW_int, KW_kernel, KW_load_image, KW_load_sequence, KW_mat2, KW_mat3, KW_mat4, KW_nan, KW_new, KW_output, KW_print, KW_real, KW_return, KW_stabilize, KW_start, KW_strand, KW_string, KW_tensor, KW_true, KW_update, KW_vec2, KW_vec3, KW_vec4, KW_zeros, KW_OField, KW_fem, KW_mesh, KW_element, KW_fnspace, OP_eq, OP_pluseq, OP_minuseq, OP_stareq, OP_slasheq, OP_modeq, OP_orelse, OP_andalso, OP_lt, OP_lte, OP_eqeq, OP_neq, OP_gte, OP_gt, OP_plus, OP_minus, OP_star, OP_convolve, OP_dot, OP_cross, OP_outer, OP_slash, OP_mod, OP_exp, OP_comp, OP_at, OP_D, OP_Dotimes, OP_curl, OP_Ddot, LP, RP, LB, RB, LCB, RCB, COMMA, SEMI, COLON, HASH, BANG, BAR, DOT, DOTDOT, EOF
           ]
    fun toString tok =
(case (tok)
 of (KW_bool) => "bool"
  | (KW_const) => "const"
  | (KW_continue) => "continue"
  | (KW_create_array) => "create_array"
  | (KW_create_collection) => "create_collection"
  | (KW_die) => "die"
  | (KW_else) => "else"
  | (KW_false) => "false"
  | (KW_field) => "field"
  | (KW_foreach) => "foreach"
  | (KW_function) => "function"
  | (KW_identity) => "identity"
  | (KW_if) => "if"
  | (KW_image) => "image"
  | (KW_in) => "in"
  | (KW_initialize) => "initialize"
  | (KW_input) => "input"
  | (KW_int) => "int"
  | (KW_kernel) => "kernel"
  | (KW_load_image) => "load_image"
  | (KW_load_sequence) => "load_sequence"
  | (KW_mat2) => "mat2"
  | (KW_mat3) => "mat3"
  | (KW_mat4) => "mat4"
  | (KW_nan) => "nan"
  | (KW_new) => "new"
  | (KW_output) => "output"
  | (KW_print) => "print"
  | (KW_real) => "real"
  | (KW_return) => "return"
  | (KW_stabilize) => "stabilize"
  | (KW_start) => "start"
  | (KW_strand) => "strand"
  | (KW_string) => "string"
  | (KW_tensor) => "tensor"
  | (KW_true) => "true"
  | (KW_update) => "update"
  | (KW_vec2) => "vec2"
  | (KW_vec3) => "vec3"
  | (KW_vec4) => "vec4"
  | (KW_zeros) => "zeros"
  | (KW_OField) => "ofield"
  | (KW_fem) => "fem"
  | (KW_mesh) => "mesh"
  | (KW_element) => "element"
  | (KW_fnspace) => "fnspace"
  | (OP_eq) => "="
  | (OP_pluseq) => "+="
  | (OP_minuseq) => "-="
  | (OP_stareq) => "*="
  | (OP_slasheq) => "/="
  | (OP_modeq) => "%="
  | (OP_orelse) => "||"
  | (OP_andalso) => "&&"
  | (OP_lt) => "<"
  | (OP_lte) => "<="
  | (OP_eqeq) => "=="
  | (OP_neq) => "!="
  | (OP_gte) => ">="
  | (OP_gt) => ">"
  | (OP_plus) => "+"
  | (OP_minus) => "-"
  | (OP_star) => "*"
  | (OP_convolve) => "\226\138\155"
  | (OP_dot) => "\226\128\162"
  | (OP_cross) => "\195\151"
  | (OP_outer) => "\226\138\151"
  | (OP_slash) => "/"
  | (OP_mod) => "%"
  | (OP_exp) => "^"
  | (OP_comp) => "\226\136\152"
  | (OP_at) => "@"
  | (OP_D) => "\226\136\135"
  | (OP_Dotimes) => "\226\136\135\226\138\151"
  | (OP_curl) => "\226\136\135\195\151"
  | (OP_Ddot) => "\226\136\135\226\128\162"
  | (LP) => "("
  | (RP) => ")"
  | (LB) => "["
  | (RB) => "]"
  | (LCB) => "{"
  | (RCB) => "}"
  | (COMMA) => ","
  | (SEMI) => ";"
  | (COLON) => ":"
  | (HASH) => "#"
  | (BANG) => "!"
  | (BAR) => "|"
  | (DOT) => "."
  | (DOTDOT) => ".."
  | (VERSION(_)) => "VERSION"
  | (ID(_)) => "ID"
  | (INT(_)) => "INT"
  | (REAL(_)) => "REAL"
  | (STRING(_)) => "STRING"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (KW_bool) => true
  | (KW_const) => true
  | (KW_continue) => true
  | (KW_create_array) => true
  | (KW_create_collection) => true
  | (KW_die) => true
  | (KW_else) => true
  | (KW_false) => true
  | (KW_field) => true
  | (KW_foreach) => true
  | (KW_function) => true
  | (KW_identity) => true
  | (KW_if) => true
  | (KW_image) => true
  | (KW_in) => true
  | (KW_initialize) => true
  | (KW_input) => true
  | (KW_int) => true
  | (KW_kernel) => true
  | (KW_load_image) => true
  | (KW_load_sequence) => true
  | (KW_mat2) => true
  | (KW_mat3) => true
  | (KW_mat4) => true
  | (KW_nan) => true
  | (KW_new) => true
  | (KW_output) => true
  | (KW_print) => true
  | (KW_real) => true
  | (KW_return) => true
  | (KW_stabilize) => true
  | (KW_start) => true
  | (KW_strand) => true
  | (KW_string) => true
  | (KW_tensor) => true
  | (KW_true) => true
  | (KW_update) => true
  | (KW_vec2) => true
  | (KW_vec3) => true
  | (KW_vec4) => true
  | (KW_zeros) => true
  | (KW_OField) => true
  | (KW_fem) => true
  | (KW_mesh) => true
  | (KW_element) => true
  | (KW_fnspace) => true
  | (OP_eq) => false
  | (OP_pluseq) => false
  | (OP_minuseq) => false
  | (OP_stareq) => false
  | (OP_slasheq) => false
  | (OP_modeq) => false
  | (OP_orelse) => false
  | (OP_andalso) => false
  | (OP_lt) => false
  | (OP_lte) => false
  | (OP_eqeq) => false
  | (OP_neq) => false
  | (OP_gte) => false
  | (OP_gt) => false
  | (OP_plus) => false
  | (OP_minus) => false
  | (OP_star) => false
  | (OP_convolve) => false
  | (OP_dot) => false
  | (OP_cross) => false
  | (OP_outer) => false
  | (OP_slash) => false
  | (OP_mod) => false
  | (OP_exp) => false
  | (OP_comp) => false
  | (OP_at) => false
  | (OP_D) => false
  | (OP_Dotimes) => false
  | (OP_curl) => false
  | (OP_Ddot) => false
  | (LP) => false
  | (RP) => false
  | (LB) => false
  | (RB) => false
  | (LCB) => false
  | (RCB) => false
  | (COMMA) => false
  | (SEMI) => false
  | (COLON) => false
  | (HASH) => false
  | (BANG) => false
  | (BAR) => false
  | (DOT) => false
  | (DOTDOT) => false
  | (VERSION(_)) => false
  | (ID(_)) => false
  | (INT(_)) => false
  | (REAL(_)) => false
  | (STRING(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* DiderotTokens *)

functor DiderotParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
DiderotTokens
    structure UserCode =
      struct

  structure PT = ParseTree
  structure L = Literal
  structure Op = Operators


  fun mark cons (span : AntlrStreamPos.span, tr) = cons{span = span, tree = tr}


  val markDecl = mark PT.GD_Mark
  fun markTy (_, e as PT.T_Mark _) = e
    | markTy (sp, tr) = mark PT.T_Mark (sp, tr)
  fun markStmt (_, e as PT.S_Mark _) = e
    | markStmt (sp, tr) = mark PT.S_Mark (sp, tr)
  fun markExpr (_, e as PT.E_Mark _) = e
    | markExpr (sp, tr) = mark PT.E_Mark (sp, tr)

  fun mkCondExp cons = let
        fun mk (_, e, [], _) = e
          | mk (lpos, e, [(_, e')], rpos) = markExpr((lpos, rpos), cons(e, e'))
          | mk (lpos, e, (pos, e')::r, rpos) = markExpr((lpos, rpos), cons(e, mk(pos, e', r, rpos)))
        in
          mk
        end


  fun mkBinApp (e1, rator, e2) = PT.E_BinOp(e1, rator, e2)


  fun mkLBinExp (_, e, []) = e
    | mkLBinExp (lpos, e, (id, e', rpos)::r) =
        mkLBinExp (lpos, markExpr((lpos, rpos), mkBinApp(e, id, e')), r)


  fun mkRBinExp (_, e, [], _) = e
    | mkRBinExp (lpos, e, [(id, _, e')], rpos) =
        markExpr ((lpos, rpos), mkBinApp(e, id, e'))
    | mkRBinExp (lpos, e, (id, pos, e')::r, rpos) =
        markExpr ((lpos, rpos), mkBinApp(e, id, mkRBinExp(pos, e', r, rpos)))

  fun mkOptExp (_, e, NONE) = e
    | mkOptExp (spn, e, SOME mk) = mk(spn, e)

  fun flatten NONE = []
    | flatten (SOME(x, xs)) = x::xs

  fun ilit i = PT.E_Lit(L.Int i)

fun Root_PROD_1_ACT (VERSION, Program, VERSION_SPAN : (Lex.pos * Lex.pos), Program_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.Program{span = Program_SPAN, tree = Program})
fun Program_PROD_1_ACT (GlobalStart, GlobalDcl, InitializeBlock, StrandDcl, GlobalUpdate, CreateStrands, GlobalStart_SPAN : (Lex.pos * Lex.pos), GlobalDcl_SPAN : (Lex.pos * Lex.pos), InitializeBlock_SPAN : (Lex.pos * Lex.pos), StrandDcl_SPAN : (Lex.pos * Lex.pos), GlobalUpdate_SPAN : (Lex.pos * Lex.pos), CreateStrands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({
                      globals = GlobalDcl,
                      globInit = InitializeBlock,
                      strand = StrandDcl,
                      create = CreateStrands,
                      start = GlobalStart,
                      update = GlobalUpdate
                    })
fun CreateStrands_PROD_1_ACT (SEMI, Comprehension, KW_create_collection, SEMI_SPAN : (Lex.pos * Lex.pos), Comprehension_SPAN : (Lex.pos * Lex.pos), KW_create_collection_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.CR_Mark (FULL_SPAN, PT.CR_Collection Comprehension))
fun CreateStrands_PROD_2_ACT (SEMI, Comprehension, KW_create_array, SEMI_SPAN : (Lex.pos * Lex.pos), Comprehension_SPAN : (Lex.pos * Lex.pos), KW_create_array_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.CR_Mark (FULL_SPAN, PT.CR_Array(NONE, Comprehension)))
fun GlobalDcl_PROD_1_ACT (InputDcl, InputDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (InputDcl)
fun GlobalDcl_PROD_2_ACT (ConstDcl, ConstDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ConstDcl)
fun GlobalDcl_PROD_3_ACT (VarDcl, VarDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markDecl(FULL_SPAN, PT.GD_Var VarDcl))
fun GlobalDcl_PROD_4_ACT (FunctionDcl, FunctionDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (FunctionDcl)
fun ConstDcl_PROD_1_ACT (SR, SEMI, Type, KW_const, BindId, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_const_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markDecl(FULL_SPAN, PT.GD_Const(Type, BindId, SR)))
fun InputDcl_PROD_1_ACT (SR1, SR2, SEMI, Type, KW_input, BindId, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_input_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markDecl(FULL_SPAN, PT.GD_Input(Type, BindId, SR1, SR2)))
fun VarDcl_PROD_1_ACT (SR, SEMI, Type, BindId, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.VD_Mark (FULL_SPAN, PT.VD_Decl(Type, BindId, SR)))
fun FunctionDcl_PROD_1_ACT (LP, RP, Type, Parameters, KW_function, BindId, FunctionDef, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), Parameters_SPAN : (Lex.pos * Lex.pos), KW_function_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FunctionDef_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markDecl(FULL_SPAN, PT.GD_Func(Type, BindId, Parameters, FunctionDef)))
fun Parameters_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (flatten SR)
fun Parameter_PROD_1_ACT (Type, BindId, Type_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.P_Mark (FULL_SPAN, PT.P_Param(Type, BindId)))
fun FunctionDef_PROD_1_ACT (SEMI, Expression, OP_eq, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.FB_Expr Expression)
fun FunctionDef_PROD_2_ACT (SEMI, Block, SEMI_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.FB_Stmt Block)
fun StrandDcl_PROD_1_ACT (LP, RP, LCB, RCB, SEMI, Parameters, InitializeBlock, StateVarDcl, BindId, KW_strand, MethodDcl, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Parameters_SPAN : (Lex.pos * Lex.pos), InitializeBlock_SPAN : (Lex.pos * Lex.pos), StateVarDcl_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), KW_strand_SPAN : (Lex.pos * Lex.pos), MethodDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.SD_Mark (FULL_SPAN, PT.SD_Strand{
                      name = BindId,
                      params = Parameters,
                      state = StateVarDcl,
                      stateInit = InitializeBlock,
                      methods = MethodDcl
                    }))
fun StateVarDcl_PROD_1_ACT (VarDcl, KW_output, VarDcl_SPAN : (Lex.pos * Lex.pos), KW_output_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.SVD_Mark (FULL_SPAN, PT.SVD_VarDcl(true, VarDcl)))
fun StateVarDcl_PROD_2_ACT (VarDcl, VarDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.SVD_Mark (FULL_SPAN, PT.SVD_VarDcl(false, VarDcl)))
fun MethodDcl_PROD_1_ACT (SEMI, Block, MethodName, SEMI_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), MethodName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.M_Mark (FULL_SPAN, PT.M_Method(MethodName, Block)))
fun MethodName_PROD_1_ACT (KW_start, KW_start_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (StrandUtil.Start)
fun MethodName_PROD_2_ACT (KW_update, KW_update_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (StrandUtil.Update)
fun MethodName_PROD_3_ACT (KW_stabilize, KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (StrandUtil.Stabilize)
fun GlobalStart_PROD_1_ACT (SEMI, KW_start, Block, SEMI_SPAN : (Lex.pos * Lex.pos), KW_start_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, Block))
fun GlobalUpdate_PROD_1_ACT (SEMI, Block, KW_update, SEMI_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), KW_update_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, Block))
fun Block_PROD_1_ACT (SR, LCB, RCB, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of [s] => markStmt (FULL_SPAN, s)
                      | stms => markStmt (FULL_SPAN, PT.S_Block stms)
                    )
fun Statement_PROD_2_ACT (LP, RP, Expression, KW_if, Statement, IfRest, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), Statement_SPAN : (Lex.pos * Lex.pos), IfRest_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (IfRest (FULL_SPAN, Expression, Statement))
fun Statement_PROD_3_ACT (LP, RP, Type, Block, KW_foreach, Iterator, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), KW_foreach_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Foreach(Type, Iterator, Block)))
fun IfRest_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn (span, e, s) => markStmt (span, PT.S_IfThen(e, s)))
fun IfRest_PROD_2_ACT (KW_else, Statement, KW_else_SPAN : (Lex.pos * Lex.pos), Statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn (span, e, s) => markStmt (span, PT.S_IfThenElse(e, s, Statement)))
fun AtomicStmt_PROD_1_ACT (Block, Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Block)
fun AtomicStmt_PROD_2_ACT (LP, RP, SEMI, KW_print, Arguments, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_print_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Print Arguments))
fun AtomicStmt_PROD_3_ACT (ID, LP, RP, SEMI, Arguments, KW_new, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), KW_new_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_New(ID, Arguments)))
fun AtomicStmt_PROD_4_ACT (SEMI, KW_stabilize, SEMI_SPAN : (Lex.pos * Lex.pos), KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Stabilize))
fun AtomicStmt_PROD_5_ACT (SEMI, KW_die, SEMI_SPAN : (Lex.pos * Lex.pos), KW_die_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Die))
fun AtomicStmt_PROD_6_ACT (SEMI, KW_continue, SEMI_SPAN : (Lex.pos * Lex.pos), KW_continue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Continue))
fun AtomicStmt_PROD_7_ACT (SEMI, Expression, KW_return, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), KW_return_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Return Expression))
fun AtomicStmt_PROD_8_ACT (VarDcl, VarDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.S_Decl VarDcl)
fun AtomicStmt_PROD_9_ACT (SEMI, Expression, OP_eq, BindId, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Assign(BindId, NONE, Expression)))
fun AtomicStmt_PROD_10_ACT (SEMI, Expression, AssignOp, BindId, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), AssignOp_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Assign(BindId, SOME AssignOp, Expression)))
fun AssignOp_PROD_1_ACT (OP_pluseq, OP_pluseq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.asgn_add)
fun AssignOp_PROD_2_ACT (OP_minuseq, OP_minuseq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.asgn_sub)
fun AssignOp_PROD_3_ACT (OP_stareq, OP_stareq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.asgn_mul)
fun AssignOp_PROD_4_ACT (OP_slasheq, OP_slasheq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.asgn_div)
fun AssignOp_PROD_5_ACT (OP_modeq, OP_modeq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.asgn_mod)
fun Type_PROD_1_ACT (LP, RP, KW_image, Shape, ConstExpr, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_Image{dim=ConstExpr, shape=Shape}))
fun Type_PROD_2_ACT (LP, RP, HASH, KW_field, Continuity, Shape, ConstExpr, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_field_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_Field{diff=Continuity, dim=ConstExpr, shape=Shape}))
fun Type_PROD_3_ACT (LP, RP, HASH, KW_OField, Continuity, Shape, ConstExpr, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_OField_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_OField{diff=Continuity, dim=ConstExpr, shape=Shape}))
fun Type_PROD_4_ACT (LP, RP, HASH, Continuity, Shape, ConstExpr, KW_fem, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), KW_fem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_FemFld{diff=Continuity, dim=ConstExpr, shape=Shape}))
fun Type_PROD_5_ACT (KW_mesh, KW_mesh_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_Mesh))
fun Type_PROD_6_ACT (KW_element, KW_element_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_Element))
fun Type_PROD_7_ACT (KW_fnspace, KW_fnspace_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_FnSpace))
fun Type_PROD_8_ACT (HASH, Continuity, KW_kernel, HASH_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), KW_kernel_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_Kernel Continuity))
fun Type_PROD_9_ACT (ConcreteType, ConcreteType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ConcreteType)
fun Continuity_PROD_1_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (INT)
fun ConcreteType_PROD_1_ACT (SequenceDims, PrimitiveType, SequenceDims_SPAN : (Lex.pos * Lex.pos), PrimitiveType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, SequenceDims PrimitiveType))
fun SequenceDims_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn ty => ty)
fun SequenceDims_PROD_2_ACT (LB, RB, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn ty => PT.T_DynSeq ty)
fun SequenceDims_PROD_3_ACT (LB, RB, SequenceDims, ConstExpr, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SequenceDims_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn ty => SequenceDims(PT.T_Seq(ty, ConstExpr)))
fun PrimitiveType_PROD_1_ACT (Shape, KW_tensor, Shape_SPAN : (Lex.pos * Lex.pos), KW_tensor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Tensor Shape))
fun PrimitiveType_PROD_2_ACT (KW_vec2, KW_vec2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 2])
fun PrimitiveType_PROD_3_ACT (KW_vec3, KW_vec3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 3])
fun PrimitiveType_PROD_4_ACT (KW_vec4, KW_vec4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 4])
fun PrimitiveType_PROD_5_ACT (KW_mat2, KW_mat2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 2, ilit 2])
fun PrimitiveType_PROD_6_ACT (KW_mat3, KW_mat3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 3, ilit 3])
fun PrimitiveType_PROD_7_ACT (KW_mat4, KW_mat4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 4, ilit 4])
fun PrimitiveType_PROD_8_ACT (KW_bool, KW_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Bool)
fun PrimitiveType_PROD_9_ACT (KW_int, KW_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Int)
fun PrimitiveType_PROD_10_ACT (KW_real, KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[])
fun PrimitiveType_PROD_11_ACT (KW_string, KW_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_String)
fun PrimitiveType_PROD_12_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Id ID)
fun Shape_PROD_1_ACT (LB, RB, SR, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (flatten (SR : (PT.expr * PT.expr list) option))
fun Comprehension_PROD_1_ACT (SR, BAR, LCB, RCB, Expression, Iterator, SR_SPAN : (Lex.pos * Lex.pos), BAR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.COMP_Mark
                      (FULL_SPAN, PT.COMP_Comprehension(Expression, Iterator :: SR)))
fun Iterator_PROD_1_ACT (Expression, KW_in, BindId, Expression_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.I_Mark (FULL_SPAN, PT.I_Iterator(BindId, Expression)))
fun Expression_PROD_1_SUBRULE_1_PROD_1_ACT (KW_else, KW_if, RangeExpr, Expression1, Expression2, KW_else_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), RangeExpr_SPAN : (Lex.pos * Lex.pos), Expression1_SPAN : (Lex.pos * Lex.pos), Expression2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Expression1, Expression2)
fun Expression_PROD_1_ACT (SR, RangeExpr, SR_SPAN : (Lex.pos * Lex.pos), RangeExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of NONE => RangeExpr
                      | SOME(e1, e2) => markExpr(FULL_SPAN, PT.E_Cond(RangeExpr, e1, e2))
                    )
fun RangeExpr_PROD_1_ACT (SR, OrExpr, SR_SPAN : (Lex.pos * Lex.pos), OrExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of NONE => OrExpr
                      | SOME e => markExpr (FULL_SPAN, PT.E_Range(OrExpr, e))
                    )
fun OrExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_orelse, AndExpr, OP_orelse_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (#1 FULL_SPAN, AndExpr)
fun OrExpr_PROD_1_ACT (SR, AndExpr, SR_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkCondExp PT.E_OrElse (#1 AndExpr_SPAN, AndExpr, SR, #2 SR_SPAN))
fun AndExpr_PROD_1_SUBRULE_1_PROD_1_ACT (CompareExpr, OP_andalso, CompareExpr_SPAN : (Lex.pos * Lex.pos), OP_andalso_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (#1 FULL_SPAN, CompareExpr)
fun AndExpr_PROD_1_ACT (SR, CompareExpr, SR_SPAN : (Lex.pos * Lex.pos), CompareExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkCondExp PT.E_AndAlso (#1 CompareExpr_SPAN, CompareExpr, SR, #2 SR_SPAN))
fun CompareExpr_PROD_1_SUBRULE_1_PROD_1_ACT (CompareOp, AddExpr, CompareOp_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (CompareOp, AddExpr, #2 AddExpr_SPAN)
fun CompareExpr_PROD_1_ACT (SR, AddExpr, SR_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 AddExpr_SPAN, AddExpr, SR))
fun CompareOp_PROD_1_ACT (OP_lt, OP_lt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_lt)
fun CompareOp_PROD_2_ACT (OP_lte, OP_lte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_lte)
fun CompareOp_PROD_3_ACT (OP_eqeq, OP_eqeq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_equ)
fun CompareOp_PROD_4_ACT (OP_neq, OP_neq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_neq)
fun CompareOp_PROD_5_ACT (OP_gte, OP_gte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_gte)
fun CompareOp_PROD_6_ACT (OP_gt, OP_gt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_gt)
fun AddExpr_PROD_1_SUBRULE_1_PROD_1_ACT (AddOp, MultiplyExpr, AddOp_SPAN : (Lex.pos * Lex.pos), MultiplyExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AddOp, MultiplyExpr, #2 MultiplyExpr_SPAN)
fun AddExpr_PROD_1_ACT (SR, MultiplyExpr, SR_SPAN : (Lex.pos * Lex.pos), MultiplyExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 MultiplyExpr_SPAN, MultiplyExpr, SR))
fun AddOp_PROD_1_ACT (OP_plus, OP_plus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_add)
fun AddOp_PROD_2_ACT (OP_minus, OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_sub)
fun AddOp_PROD_3_ACT (OP_at, OP_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_at)
fun MultiplyExpr_PROD_1_SUBRULE_1_PROD_1_ACT (MultiplyOp, PrefixExpr, MultiplyOp_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (MultiplyOp, PrefixExpr, #2 PrefixExpr_SPAN)
fun MultiplyExpr_PROD_1_ACT (SR, PrefixExpr, SR_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 PrefixExpr_SPAN, PrefixExpr, SR))
fun MultiplyOp_PROD_1_ACT (OP_star, OP_star_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_mul)
fun MultiplyOp_PROD_2_ACT (OP_slash, OP_slash_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_div)
fun MultiplyOp_PROD_3_ACT (OP_mod, OP_mod_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_mod)
fun MultiplyOp_PROD_4_ACT (OP_convolve, OP_convolve_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_convolve)
fun MultiplyOp_PROD_5_ACT (OP_dot, OP_dot_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_dot)
fun MultiplyOp_PROD_6_ACT (OP_cross, OP_cross_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_cross)
fun MultiplyOp_PROD_7_ACT (OP_outer, OP_outer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_outer)
fun MultiplyOp_PROD_8_ACT (COLON, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_colon)
fun MultiplyOp_PROD_9_ACT (OP_comp, OP_comp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_comp)
fun PrefixExpr_PROD_1_ACT (PowerExpr, PowerExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PowerExpr)
fun PrefixExpr_PROD_2_ACT (PrefixExpr, PrefixOp, PrefixExpr_SPAN : (Lex.pos * Lex.pos), PrefixOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_UnaryOp(PrefixOp, PrefixExpr)))
fun PrefixOp_PROD_1_ACT (OP_minus, OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_neg)
fun PrefixOp_PROD_2_ACT (BANG, BANG_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_not)
fun PowerExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_exp, SuffixExpr, OP_exp_SPAN : (Lex.pos * Lex.pos), SuffixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_pow, #1 SuffixExpr_SPAN, SuffixExpr)
fun PowerExpr_PROD_1_ACT (SR, SuffixExpr, SR_SPAN : (Lex.pos * Lex.pos), SuffixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkRBinExp (#1 FULL_SPAN, SuffixExpr, SR, #2 FULL_SPAN))
fun SuffixExpr_PROD_1_ACT (BAR, LCB, RCB, Expression, DiffExpr, Iterator, BAR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), DiffExpr_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN,
                      PT.E_Apply(
                        DiffExpr,
                        [PT.E_SeqComp(PT.COMP_Comprehension(Expression, [Iterator]))])))
fun SuffixExpr_PROD_2_ACT (SR, DiffExpr, SR_SPAN : (Lex.pos * Lex.pos), DiffExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of [] => DiffExpr
                      | ss => markExpr(FULL_SPAN, List.foldl (fn (f, e) => f e) DiffExpr ss)
                    )
fun SuffixExpr_PROD_3_ACT (LP, RP, Expression, KW_real, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Real Expression))
fun SuffixExpr_PROD_4_ACT (LP, RP, KW_load_sequence, ConstExpr, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_load_sequence_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_LoadSeq ConstExpr))
fun SuffixExpr_PROD_5_ACT (LP, RP, KW_load_image, ConstExpr, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_load_image_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_LoadImage ConstExpr))
fun SuffixExpr_PROD_6_ACT (LB, RB, KW_identity, ConstExpr, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), KW_identity_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Id ConstExpr))
fun SuffixExpr_PROD_7_ACT (KW_zeros, Shape, KW_zeros_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Zero Shape))
fun SuffixExpr_PROD_8_ACT (SR, KW_nan, SR_SPAN : (Lex.pos * Lex.pos), KW_nan_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_NaN(getOpt(SR, []))))
fun Suffix_PROD_1_ACT (LP, RP, Arguments, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => PT.E_Apply(e, Arguments))
fun Suffix_PROD_2_ACT (LB, RB, Indices, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Indices_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => PT.E_Subscript(e, Indices))
fun Suffix_PROD_3_ACT (ID, DOT, ID_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => PT.E_Select(e, ID))
fun Indices_PROD_1_ACT (SR, IndexExpr, SR_SPAN : (Lex.pos * Lex.pos), IndexExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (IndexExpr :: SR)
fun IndexExpr_PROD_1_ACT (Expression, Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SOME Expression)
fun IndexExpr_PROD_2_ACT (COLON, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NONE)
fun DiffExpr_PROD_1_ACT (AtomicExpr, AtomicExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AtomicExpr)
fun DiffExpr_PROD_2_ACT (DiffExpr, DiffOp, DiffExpr_SPAN : (Lex.pos * Lex.pos), DiffOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_UnaryOp(DiffOp, DiffExpr)))
fun DiffOp_PROD_1_ACT (OP_D, OP_D_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_D)
fun DiffOp_PROD_2_ACT (OP_Dotimes, OP_Dotimes_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_Dotimes)
fun DiffOp_PROD_3_ACT (OP_curl, OP_curl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_curl)
fun DiffOp_PROD_4_ACT (OP_Ddot, OP_Ddot_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_Ddot)
fun AtomicExpr_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Var ID))
fun AtomicExpr_PROD_2_ACT (ID, HASH, Continuity, ID_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Kernel(ID, Continuity)))
fun AtomicExpr_PROD_3_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, ilit INT))
fun AtomicExpr_PROD_4_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Lit(L.Real REAL)))
fun AtomicExpr_PROD_5_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Lit(L.String STRING)))
fun AtomicExpr_PROD_6_ACT (KW_true, KW_true_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Lit(L.Bool true)))
fun AtomicExpr_PROD_7_ACT (KW_false, KW_false_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Lit(L.Bool false)))
fun AtomicExpr_PROD_8_ACT (LP, RP, Expression, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Expression)
fun AtomicExpr_PROD_9_ACT (LCB, RCB, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Sequence[]))
fun AtomicExpr_PROD_10_ACT (LCB, RCB, Expression, SeqRest, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), SeqRest_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, SeqRest Expression))
fun AtomicExpr_PROD_11_ACT (LB, RB, SR, Expression, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Cons(Expression::SR)))
fun AtomicExpr_PROD_12_ACT (BAR1, BAR2, Expression, BAR1_SPAN : (Lex.pos * Lex.pos), BAR2_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_UnaryOp(BasisNames.op_norm, Expression)))
fun Arguments_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ([])
fun Arguments_PROD_2_ACT (SR, Expression, SR_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Expression :: SR)
fun SeqRest_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => PT.E_Sequence(e::SR))
fun SeqRest_PROD_2_ACT (BAR, Iterator, BAR_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => PT.E_SeqComp(PT.COMP_Comprehension(e, [Iterator])))
fun ConstExpr_PROD_1_ACT (Expression, Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Expression)
fun BindId_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({span=ID_SPAN, tree=ID})
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchKW_bool strm = (case (lex(strm))
 of (Tok.KW_bool, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_const strm = (case (lex(strm))
 of (Tok.KW_const, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_continue strm = (case (lex(strm))
 of (Tok.KW_continue, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_create_array strm = (case (lex(strm))
 of (Tok.KW_create_array, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_create_collection strm = (case (lex(strm))
 of (Tok.KW_create_collection, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_die strm = (case (lex(strm))
 of (Tok.KW_die, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_else strm = (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_false strm = (case (lex(strm))
 of (Tok.KW_false, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_field strm = (case (lex(strm))
 of (Tok.KW_field, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_foreach strm = (case (lex(strm))
 of (Tok.KW_foreach, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_function strm = (case (lex(strm))
 of (Tok.KW_function, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_identity strm = (case (lex(strm))
 of (Tok.KW_identity, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_if strm = (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_image strm = (case (lex(strm))
 of (Tok.KW_image, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_in strm = (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_initialize strm = (case (lex(strm))
 of (Tok.KW_initialize, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_input strm = (case (lex(strm))
 of (Tok.KW_input, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_int strm = (case (lex(strm))
 of (Tok.KW_int, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_kernel strm = (case (lex(strm))
 of (Tok.KW_kernel, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_load_image strm = (case (lex(strm))
 of (Tok.KW_load_image, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_load_sequence strm = (case (lex(strm))
 of (Tok.KW_load_sequence, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_mat2 strm = (case (lex(strm))
 of (Tok.KW_mat2, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_mat3 strm = (case (lex(strm))
 of (Tok.KW_mat3, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_mat4 strm = (case (lex(strm))
 of (Tok.KW_mat4, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_nan strm = (case (lex(strm))
 of (Tok.KW_nan, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_new strm = (case (lex(strm))
 of (Tok.KW_new, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_output strm = (case (lex(strm))
 of (Tok.KW_output, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_print strm = (case (lex(strm))
 of (Tok.KW_print, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_real strm = (case (lex(strm))
 of (Tok.KW_real, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_return strm = (case (lex(strm))
 of (Tok.KW_return, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_stabilize strm = (case (lex(strm))
 of (Tok.KW_stabilize, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_start strm = (case (lex(strm))
 of (Tok.KW_start, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_strand strm = (case (lex(strm))
 of (Tok.KW_strand, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_string strm = (case (lex(strm))
 of (Tok.KW_string, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_tensor strm = (case (lex(strm))
 of (Tok.KW_tensor, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_true strm = (case (lex(strm))
 of (Tok.KW_true, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_update strm = (case (lex(strm))
 of (Tok.KW_update, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_vec2 strm = (case (lex(strm))
 of (Tok.KW_vec2, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_vec3 strm = (case (lex(strm))
 of (Tok.KW_vec3, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_vec4 strm = (case (lex(strm))
 of (Tok.KW_vec4, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_zeros strm = (case (lex(strm))
 of (Tok.KW_zeros, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_OField strm = (case (lex(strm))
 of (Tok.KW_OField, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_fem strm = (case (lex(strm))
 of (Tok.KW_fem, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_mesh strm = (case (lex(strm))
 of (Tok.KW_mesh, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_element strm = (case (lex(strm))
 of (Tok.KW_element, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_fnspace strm = (case (lex(strm))
 of (Tok.KW_fnspace, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_eq strm = (case (lex(strm))
 of (Tok.OP_eq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_pluseq strm = (case (lex(strm))
 of (Tok.OP_pluseq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_minuseq strm = (case (lex(strm))
 of (Tok.OP_minuseq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_stareq strm = (case (lex(strm))
 of (Tok.OP_stareq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_slasheq strm = (case (lex(strm))
 of (Tok.OP_slasheq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_modeq strm = (case (lex(strm))
 of (Tok.OP_modeq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_orelse strm = (case (lex(strm))
 of (Tok.OP_orelse, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_andalso strm = (case (lex(strm))
 of (Tok.OP_andalso, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_lt strm = (case (lex(strm))
 of (Tok.OP_lt, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_lte strm = (case (lex(strm))
 of (Tok.OP_lte, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_eqeq strm = (case (lex(strm))
 of (Tok.OP_eqeq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_neq strm = (case (lex(strm))
 of (Tok.OP_neq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_gte strm = (case (lex(strm))
 of (Tok.OP_gte, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_gt strm = (case (lex(strm))
 of (Tok.OP_gt, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_plus strm = (case (lex(strm))
 of (Tok.OP_plus, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_minus strm = (case (lex(strm))
 of (Tok.OP_minus, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_star strm = (case (lex(strm))
 of (Tok.OP_star, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_convolve strm = (case (lex(strm))
 of (Tok.OP_convolve, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_dot strm = (case (lex(strm))
 of (Tok.OP_dot, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_cross strm = (case (lex(strm))
 of (Tok.OP_cross, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_outer strm = (case (lex(strm))
 of (Tok.OP_outer, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_slash strm = (case (lex(strm))
 of (Tok.OP_slash, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_mod strm = (case (lex(strm))
 of (Tok.OP_mod, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_exp strm = (case (lex(strm))
 of (Tok.OP_exp, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_comp strm = (case (lex(strm))
 of (Tok.OP_comp, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_at strm = (case (lex(strm))
 of (Tok.OP_at, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_D strm = (case (lex(strm))
 of (Tok.OP_D, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_Dotimes strm = (case (lex(strm))
 of (Tok.OP_Dotimes, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_curl strm = (case (lex(strm))
 of (Tok.OP_curl, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_Ddot strm = (case (lex(strm))
 of (Tok.OP_Ddot, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLB strm = (case (lex(strm))
 of (Tok.LB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRB strm = (case (lex(strm))
 of (Tok.RB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLCB strm = (case (lex(strm))
 of (Tok.LCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRCB strm = (case (lex(strm))
 of (Tok.RCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOLON strm = (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchHASH strm = (case (lex(strm))
 of (Tok.HASH, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBANG strm = (case (lex(strm))
 of (Tok.BANG, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBAR strm = (case (lex(strm))
 of (Tok.BAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDOT strm = (case (lex(strm))
 of (Tok.DOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDOTDOT strm = (case (lex(strm))
 of (Tok.DOTDOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchVERSION strm = (case (lex(strm))
 of (Tok.VERSION(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchINT strm = (case (lex(strm))
 of (Tok.INT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchREAL strm = (case (lex(strm))
 of (Tok.REAL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Root_NT) = 
let
fun PrefixOp_NT (strm) = let
      fun PrefixOp_PROD_1 (strm) = let
            val (OP_minus_RES, OP_minus_SPAN, strm') = matchOP_minus(strm)
            val FULL_SPAN = (#1(OP_minus_SPAN), #2(OP_minus_SPAN))
            in
              (UserCode.PrefixOp_PROD_1_ACT (OP_minus_RES, OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrefixOp_PROD_2 (strm) = let
            val (BANG_RES, BANG_SPAN, strm') = matchBANG(strm)
            val FULL_SPAN = (#1(BANG_SPAN), #2(BANG_SPAN))
            in
              (UserCode.PrefixOp_PROD_2_ACT (BANG_RES, BANG_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.BANG, _, strm') => PrefixOp_PROD_2(strm)
          | (Tok.OP_minus, _, strm') => PrefixOp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun DiffOp_NT (strm) = let
      fun DiffOp_PROD_1 (strm) = let
            val (OP_D_RES, OP_D_SPAN, strm') = matchOP_D(strm)
            val FULL_SPAN = (#1(OP_D_SPAN), #2(OP_D_SPAN))
            in
              (UserCode.DiffOp_PROD_1_ACT (OP_D_RES, OP_D_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DiffOp_PROD_2 (strm) = let
            val (OP_Dotimes_RES, OP_Dotimes_SPAN, strm') = matchOP_Dotimes(strm)
            val FULL_SPAN = (#1(OP_Dotimes_SPAN), #2(OP_Dotimes_SPAN))
            in
              (UserCode.DiffOp_PROD_2_ACT (OP_Dotimes_RES, OP_Dotimes_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DiffOp_PROD_3 (strm) = let
            val (OP_curl_RES, OP_curl_SPAN, strm') = matchOP_curl(strm)
            val FULL_SPAN = (#1(OP_curl_SPAN), #2(OP_curl_SPAN))
            in
              (UserCode.DiffOp_PROD_3_ACT (OP_curl_RES, OP_curl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DiffOp_PROD_4 (strm) = let
            val (OP_Ddot_RES, OP_Ddot_SPAN, strm') = matchOP_Ddot(strm)
            val FULL_SPAN = (#1(OP_Ddot_SPAN), #2(OP_Ddot_SPAN))
            in
              (UserCode.DiffOp_PROD_4_ACT (OP_Ddot_RES, OP_Ddot_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_Ddot, _, strm') => DiffOp_PROD_4(strm)
          | (Tok.OP_Dotimes, _, strm') => DiffOp_PROD_2(strm)
          | (Tok.OP_D, _, strm') => DiffOp_PROD_1(strm)
          | (Tok.OP_curl, _, strm') => DiffOp_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
fun Continuity_NT (strm) = let
      val (INT_RES, INT_SPAN, strm') = matchINT(strm)
      val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
      in
        (UserCode.Continuity_PROD_1_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun MultiplyOp_NT (strm) = let
      fun MultiplyOp_PROD_1 (strm) = let
            val (OP_star_RES, OP_star_SPAN, strm') = matchOP_star(strm)
            val FULL_SPAN = (#1(OP_star_SPAN), #2(OP_star_SPAN))
            in
              (UserCode.MultiplyOp_PROD_1_ACT (OP_star_RES, OP_star_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_2 (strm) = let
            val (OP_slash_RES, OP_slash_SPAN, strm') = matchOP_slash(strm)
            val FULL_SPAN = (#1(OP_slash_SPAN), #2(OP_slash_SPAN))
            in
              (UserCode.MultiplyOp_PROD_2_ACT (OP_slash_RES, OP_slash_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_3 (strm) = let
            val (OP_mod_RES, OP_mod_SPAN, strm') = matchOP_mod(strm)
            val FULL_SPAN = (#1(OP_mod_SPAN), #2(OP_mod_SPAN))
            in
              (UserCode.MultiplyOp_PROD_3_ACT (OP_mod_RES, OP_mod_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_4 (strm) = let
            val (OP_convolve_RES, OP_convolve_SPAN, strm') = matchOP_convolve(strm)
            val FULL_SPAN = (#1(OP_convolve_SPAN), #2(OP_convolve_SPAN))
            in
              (UserCode.MultiplyOp_PROD_4_ACT (OP_convolve_RES, OP_convolve_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_5 (strm) = let
            val (OP_dot_RES, OP_dot_SPAN, strm') = matchOP_dot(strm)
            val FULL_SPAN = (#1(OP_dot_SPAN), #2(OP_dot_SPAN))
            in
              (UserCode.MultiplyOp_PROD_5_ACT (OP_dot_RES, OP_dot_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_6 (strm) = let
            val (OP_cross_RES, OP_cross_SPAN, strm') = matchOP_cross(strm)
            val FULL_SPAN = (#1(OP_cross_SPAN), #2(OP_cross_SPAN))
            in
              (UserCode.MultiplyOp_PROD_6_ACT (OP_cross_RES, OP_cross_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_7 (strm) = let
            val (OP_outer_RES, OP_outer_SPAN, strm') = matchOP_outer(strm)
            val FULL_SPAN = (#1(OP_outer_SPAN), #2(OP_outer_SPAN))
            in
              (UserCode.MultiplyOp_PROD_7_ACT (OP_outer_RES, OP_outer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_8 (strm) = let
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm)
            val FULL_SPAN = (#1(COLON_SPAN), #2(COLON_SPAN))
            in
              (UserCode.MultiplyOp_PROD_8_ACT (COLON_RES, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyOp_PROD_9 (strm) = let
            val (OP_comp_RES, OP_comp_SPAN, strm') = matchOP_comp(strm)
            val FULL_SPAN = (#1(OP_comp_SPAN), #2(OP_comp_SPAN))
            in
              (UserCode.MultiplyOp_PROD_9_ACT (OP_comp_RES, OP_comp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_comp, _, strm') => MultiplyOp_PROD_9(strm)
          | (Tok.OP_outer, _, strm') => MultiplyOp_PROD_7(strm)
          | (Tok.OP_dot, _, strm') => MultiplyOp_PROD_5(strm)
          | (Tok.OP_mod, _, strm') => MultiplyOp_PROD_3(strm)
          | (Tok.OP_star, _, strm') => MultiplyOp_PROD_1(strm)
          | (Tok.OP_slash, _, strm') => MultiplyOp_PROD_2(strm)
          | (Tok.OP_convolve, _, strm') => MultiplyOp_PROD_4(strm)
          | (Tok.OP_cross, _, strm') => MultiplyOp_PROD_6(strm)
          | (Tok.COLON, _, strm') => MultiplyOp_PROD_8(strm)
          | _ => fail()
        (* end case *))
      end
fun AddOp_NT (strm) = let
      fun AddOp_PROD_1 (strm) = let
            val (OP_plus_RES, OP_plus_SPAN, strm') = matchOP_plus(strm)
            val FULL_SPAN = (#1(OP_plus_SPAN), #2(OP_plus_SPAN))
            in
              (UserCode.AddOp_PROD_1_ACT (OP_plus_RES, OP_plus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AddOp_PROD_2 (strm) = let
            val (OP_minus_RES, OP_minus_SPAN, strm') = matchOP_minus(strm)
            val FULL_SPAN = (#1(OP_minus_SPAN), #2(OP_minus_SPAN))
            in
              (UserCode.AddOp_PROD_2_ACT (OP_minus_RES, OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AddOp_PROD_3 (strm) = let
            val (OP_at_RES, OP_at_SPAN, strm') = matchOP_at(strm)
            val FULL_SPAN = (#1(OP_at_SPAN), #2(OP_at_SPAN))
            in
              (UserCode.AddOp_PROD_3_ACT (OP_at_RES, OP_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_at, _, strm') => AddOp_PROD_3(strm)
          | (Tok.OP_plus, _, strm') => AddOp_PROD_1(strm)
          | (Tok.OP_minus, _, strm') => AddOp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun CompareOp_NT (strm) = let
      fun CompareOp_PROD_1 (strm) = let
            val (OP_lt_RES, OP_lt_SPAN, strm') = matchOP_lt(strm)
            val FULL_SPAN = (#1(OP_lt_SPAN), #2(OP_lt_SPAN))
            in
              (UserCode.CompareOp_PROD_1_ACT (OP_lt_RES, OP_lt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CompareOp_PROD_2 (strm) = let
            val (OP_lte_RES, OP_lte_SPAN, strm') = matchOP_lte(strm)
            val FULL_SPAN = (#1(OP_lte_SPAN), #2(OP_lte_SPAN))
            in
              (UserCode.CompareOp_PROD_2_ACT (OP_lte_RES, OP_lte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CompareOp_PROD_3 (strm) = let
            val (OP_eqeq_RES, OP_eqeq_SPAN, strm') = matchOP_eqeq(strm)
            val FULL_SPAN = (#1(OP_eqeq_SPAN), #2(OP_eqeq_SPAN))
            in
              (UserCode.CompareOp_PROD_3_ACT (OP_eqeq_RES, OP_eqeq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CompareOp_PROD_4 (strm) = let
            val (OP_neq_RES, OP_neq_SPAN, strm') = matchOP_neq(strm)
            val FULL_SPAN = (#1(OP_neq_SPAN), #2(OP_neq_SPAN))
            in
              (UserCode.CompareOp_PROD_4_ACT (OP_neq_RES, OP_neq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CompareOp_PROD_5 (strm) = let
            val (OP_gte_RES, OP_gte_SPAN, strm') = matchOP_gte(strm)
            val FULL_SPAN = (#1(OP_gte_SPAN), #2(OP_gte_SPAN))
            in
              (UserCode.CompareOp_PROD_5_ACT (OP_gte_RES, OP_gte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CompareOp_PROD_6 (strm) = let
            val (OP_gt_RES, OP_gt_SPAN, strm') = matchOP_gt(strm)
            val FULL_SPAN = (#1(OP_gt_SPAN), #2(OP_gt_SPAN))
            in
              (UserCode.CompareOp_PROD_6_ACT (OP_gt_RES, OP_gt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_gt, _, strm') => CompareOp_PROD_6(strm)
          | (Tok.OP_neq, _, strm') => CompareOp_PROD_4(strm)
          | (Tok.OP_lte, _, strm') => CompareOp_PROD_2(strm)
          | (Tok.OP_lt, _, strm') => CompareOp_PROD_1(strm)
          | (Tok.OP_eqeq, _, strm') => CompareOp_PROD_3(strm)
          | (Tok.OP_gte, _, strm') => CompareOp_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
fun BindId_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
      in
        (UserCode.BindId_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Iterator_NT (strm) = let
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
      val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
      val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
      val FULL_SPAN = (#1(BindId_SPAN), #2(Expression_SPAN))
      in
        (UserCode.Iterator_PROD_1_ACT (Expression_RES, KW_in_RES, BindId_RES, Expression_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Expression_NT (strm) = let
      val (RangeExpr_RES, RangeExpr_SPAN, strm') = RangeExpr_NT(strm)
      fun Expression_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (Expression1_RES, Expression1_SPAN, strm') = Expression_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (Expression2_RES, Expression2_SPAN, strm') = Expression_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(Expression2_SPAN))
            in
              (UserCode.Expression_PROD_1_SUBRULE_1_PROD_1_ACT (KW_else_RES, KW_if_RES, RangeExpr_RES, Expression1_RES, Expression2_RES, KW_else_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), RangeExpr_SPAN : (Lex.pos * Lex.pos), Expression1_SPAN : (Lex.pos * Lex.pos), Expression2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Expression_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_if, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Expression_PROD_1_SUBRULE_1_PRED, Expression_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(RangeExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.Expression_PROD_1_ACT (SR_RES, RangeExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), RangeExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and RangeExpr_NT (strm) = let
      val (OrExpr_RES, OrExpr_SPAN, strm') = OrExpr_NT(strm)
      fun RangeExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (DOTDOT_RES, DOTDOT_SPAN, strm') = matchDOTDOT(strm)
            val (OrExpr_RES, OrExpr_SPAN, strm') = OrExpr_NT(strm')
            val FULL_SPAN = (#1(DOTDOT_SPAN), #2(OrExpr_SPAN))
            in
              ((OrExpr_RES), FULL_SPAN, strm')
            end
      fun RangeExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.DOTDOT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(RangeExpr_PROD_1_SUBRULE_1_PRED, RangeExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OrExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.RangeExpr_PROD_1_ACT (SR_RES, OrExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), OrExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OrExpr_NT (strm) = let
      val (AndExpr_RES, AndExpr_SPAN, strm') = AndExpr_NT(strm)
      fun OrExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (OP_orelse_RES, OP_orelse_SPAN, strm') = matchOP_orelse(strm)
            val (AndExpr_RES, AndExpr_SPAN, strm') = AndExpr_NT(strm')
            val FULL_SPAN = (#1(OP_orelse_SPAN), #2(AndExpr_SPAN))
            in
              (UserCode.OrExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_orelse_RES, AndExpr_RES, OP_orelse_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun OrExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_orelse, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OrExpr_PROD_1_SUBRULE_1_PRED, OrExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AndExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.OrExpr_PROD_1_ACT (SR_RES, AndExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AndExpr_NT (strm) = let
      val (CompareExpr_RES, CompareExpr_SPAN, strm') = CompareExpr_NT(strm)
      fun AndExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (OP_andalso_RES, OP_andalso_SPAN, strm') = matchOP_andalso(strm)
            val (CompareExpr_RES, CompareExpr_SPAN, strm') = CompareExpr_NT(strm')
            val FULL_SPAN = (#1(OP_andalso_SPAN), #2(CompareExpr_SPAN))
            in
              (UserCode.AndExpr_PROD_1_SUBRULE_1_PROD_1_ACT (CompareExpr_RES, OP_andalso_RES, CompareExpr_SPAN : (Lex.pos * Lex.pos), OP_andalso_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AndExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_andalso, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AndExpr_PROD_1_SUBRULE_1_PRED, AndExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(CompareExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.AndExpr_PROD_1_ACT (SR_RES, CompareExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), CompareExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and CompareExpr_NT (strm) = let
      val (AddExpr_RES, AddExpr_SPAN, strm') = AddExpr_NT(strm)
      fun CompareExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (CompareOp_RES, CompareOp_SPAN, strm') = CompareOp_NT(strm)
            val (AddExpr_RES, AddExpr_SPAN, strm') = AddExpr_NT(strm')
            val FULL_SPAN = (#1(CompareOp_SPAN), #2(AddExpr_SPAN))
            in
              (UserCode.CompareExpr_PROD_1_SUBRULE_1_PROD_1_ACT (CompareOp_RES, AddExpr_RES, CompareOp_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CompareExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_lt, _, strm') => true
              | (Tok.OP_lte, _, strm') => true
              | (Tok.OP_eqeq, _, strm') => true
              | (Tok.OP_neq, _, strm') => true
              | (Tok.OP_gte, _, strm') => true
              | (Tok.OP_gt, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(CompareExpr_PROD_1_SUBRULE_1_PRED, CompareExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AddExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.CompareExpr_PROD_1_ACT (SR_RES, AddExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AddExpr_NT (strm) = let
      val (MultiplyExpr_RES, MultiplyExpr_SPAN, strm') = MultiplyExpr_NT(strm)
      fun AddExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (AddOp_RES, AddOp_SPAN, strm') = AddOp_NT(strm)
            val (MultiplyExpr_RES, MultiplyExpr_SPAN, strm') = MultiplyExpr_NT(strm')
            val FULL_SPAN = (#1(AddOp_SPAN), #2(MultiplyExpr_SPAN))
            in
              (UserCode.AddExpr_PROD_1_SUBRULE_1_PROD_1_ACT (AddOp_RES, MultiplyExpr_RES, AddOp_SPAN : (Lex.pos * Lex.pos), MultiplyExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AddExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_plus, _, strm') => true
              | (Tok.OP_minus, _, strm') => true
              | (Tok.OP_at, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AddExpr_PROD_1_SUBRULE_1_PRED, AddExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(MultiplyExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.AddExpr_PROD_1_ACT (SR_RES, MultiplyExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), MultiplyExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and MultiplyExpr_NT (strm) = let
      val (PrefixExpr_RES, PrefixExpr_SPAN, strm') = PrefixExpr_NT(strm)
      fun MultiplyExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (MultiplyOp_RES, MultiplyOp_SPAN, strm') = MultiplyOp_NT(strm)
            val (PrefixExpr_RES, PrefixExpr_SPAN, strm') = PrefixExpr_NT(strm')
            val FULL_SPAN = (#1(MultiplyOp_SPAN), #2(PrefixExpr_SPAN))
            in
              (UserCode.MultiplyExpr_PROD_1_SUBRULE_1_PROD_1_ACT (MultiplyOp_RES, PrefixExpr_RES, MultiplyOp_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MultiplyExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_star, _, strm') => true
              | (Tok.OP_convolve, _, strm') => true
              | (Tok.OP_dot, _, strm') => true
              | (Tok.OP_cross, _, strm') => true
              | (Tok.OP_outer, _, strm') => true
              | (Tok.OP_slash, _, strm') => true
              | (Tok.OP_mod, _, strm') => true
              | (Tok.OP_comp, _, strm') => true
              | (Tok.COLON, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(MultiplyExpr_PROD_1_SUBRULE_1_PRED, MultiplyExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(PrefixExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.MultiplyExpr_PROD_1_ACT (SR_RES, PrefixExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and PrefixExpr_NT (strm) = let
      fun PrefixExpr_PROD_1 (strm) = let
            val (PowerExpr_RES, PowerExpr_SPAN, strm') = PowerExpr_NT(strm)
            val FULL_SPAN = (#1(PowerExpr_SPAN), #2(PowerExpr_SPAN))
            in
              (UserCode.PrefixExpr_PROD_1_ACT (PowerExpr_RES, PowerExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrefixExpr_PROD_2 (strm) = let
            val (PrefixOp_RES, PrefixOp_SPAN, strm') = PrefixOp_NT(strm)
            val (PrefixExpr_RES, PrefixExpr_SPAN, strm') = PrefixExpr_NT(strm')
            val FULL_SPAN = (#1(PrefixOp_SPAN), #2(PrefixExpr_SPAN))
            in
              (UserCode.PrefixExpr_PROD_2_ACT (PrefixExpr_RES, PrefixOp_RES, PrefixExpr_SPAN : (Lex.pos * Lex.pos), PrefixOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_minus, _, strm') => PrefixExpr_PROD_2(strm)
          | (Tok.BANG, _, strm') => PrefixExpr_PROD_2(strm)
          | (Tok.KW_false, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_identity, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_load_image, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_load_sequence, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_nan, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_real, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_true, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_zeros, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.OP_D, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.OP_Dotimes, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.OP_curl, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.OP_Ddot, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.LP, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.LB, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.LCB, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.BAR, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.ID(_), _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.INT(_), _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => PrefixExpr_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and PowerExpr_NT (strm) = let
      val (SuffixExpr_RES, SuffixExpr_SPAN, strm') = SuffixExpr_NT(strm)
      fun PowerExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (OP_exp_RES, OP_exp_SPAN, strm') = matchOP_exp(strm)
            val (SuffixExpr_RES, SuffixExpr_SPAN, strm') = SuffixExpr_NT(strm')
            val FULL_SPAN = (#1(OP_exp_SPAN), #2(SuffixExpr_SPAN))
            in
              (UserCode.PowerExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_exp_RES, SuffixExpr_RES, OP_exp_SPAN : (Lex.pos * Lex.pos), SuffixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PowerExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_exp, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(PowerExpr_PROD_1_SUBRULE_1_PRED, PowerExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(SuffixExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.PowerExpr_PROD_1_ACT (SR_RES, SuffixExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), SuffixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and SuffixExpr_NT (strm) = let
      fun SuffixExpr_PROD_1 (strm) = let
            val (DiffExpr_RES, DiffExpr_SPAN, strm') = DiffExpr_NT(strm)
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm')
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm')
            val (Iterator_RES, Iterator_SPAN, strm') = Iterator_NT(strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(DiffExpr_SPAN), #2(RCB_SPAN))
            in
              (UserCode.SuffixExpr_PROD_1_ACT (BAR_RES, LCB_RES, RCB_RES, Expression_RES, DiffExpr_RES, Iterator_RES, BAR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), DiffExpr_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_2 (strm) = let
            val (DiffExpr_RES, DiffExpr_SPAN, strm') = DiffExpr_NT(strm)
            fun SuffixExpr_PROD_2_SUBRULE_1_NT (strm) = let
                  val (Suffix_RES, Suffix_SPAN, strm') = Suffix_NT(strm)
                  val FULL_SPAN = (#1(Suffix_SPAN), #2(Suffix_SPAN))
                  in
                    ((Suffix_RES), FULL_SPAN, strm')
                  end
            fun SuffixExpr_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LP, _, strm') => true
                    | (Tok.LB, _, strm') => true
                    | (Tok.DOT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(SuffixExpr_PROD_2_SUBRULE_1_PRED, SuffixExpr_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(DiffExpr_SPAN), #2(SR_SPAN))
            in
              (UserCode.SuffixExpr_PROD_2_ACT (SR_RES, DiffExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), DiffExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_3 (strm) = let
            val (KW_real_RES, KW_real_SPAN, strm') = matchKW_real(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_real_SPAN), #2(RP_SPAN))
            in
              (UserCode.SuffixExpr_PROD_3_ACT (LP_RES, RP_RES, Expression_RES, KW_real_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_4 (strm) = let
            val (KW_load_sequence_RES, KW_load_sequence_SPAN, strm') = matchKW_load_sequence(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_load_sequence_SPAN), #2(RP_SPAN))
            in
              (UserCode.SuffixExpr_PROD_4_ACT (LP_RES, RP_RES, KW_load_sequence_RES, ConstExpr_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_load_sequence_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_5 (strm) = let
            val (KW_load_image_RES, KW_load_image_SPAN, strm') = matchKW_load_image(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_load_image_SPAN), #2(RP_SPAN))
            in
              (UserCode.SuffixExpr_PROD_5_ACT (LP_RES, RP_RES, KW_load_image_RES, ConstExpr_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_load_image_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_6 (strm) = let
            val (KW_identity_RES, KW_identity_SPAN, strm') = matchKW_identity(strm)
            val (LB_RES, LB_SPAN, strm') = matchLB(strm')
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(KW_identity_SPAN), #2(RB_SPAN))
            in
              (UserCode.SuffixExpr_PROD_6_ACT (LB_RES, RB_RES, KW_identity_RES, ConstExpr_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), KW_identity_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_7 (strm) = let
            val (KW_zeros_RES, KW_zeros_SPAN, strm') = matchKW_zeros(strm)
            val (Shape_RES, Shape_SPAN, strm') = Shape_NT(strm')
            val FULL_SPAN = (#1(KW_zeros_SPAN), #2(Shape_SPAN))
            in
              (UserCode.SuffixExpr_PROD_7_ACT (KW_zeros_RES, Shape_RES, KW_zeros_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_8 (strm) = let
            val (KW_nan_RES, KW_nan_SPAN, strm') = matchKW_nan(strm)
            fun SuffixExpr_PROD_8_SUBRULE_1_NT (strm) = let
                  val (Shape_RES, Shape_SPAN, strm') = Shape_NT(strm)
                  val FULL_SPAN = (#1(Shape_SPAN), #2(Shape_SPAN))
                  in
                    ((Shape_RES), FULL_SPAN, strm')
                  end
            fun SuffixExpr_PROD_8_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LB, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(SuffixExpr_PROD_8_SUBRULE_1_PRED, SuffixExpr_PROD_8_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(KW_nan_SPAN), #2(SR_SPAN))
            in
              (UserCode.SuffixExpr_PROD_8_ACT (SR_RES, KW_nan_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_nan_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_nan, _, strm') => SuffixExpr_PROD_8(strm)
          | (Tok.KW_identity, _, strm') => SuffixExpr_PROD_6(strm)
          | (Tok.KW_load_sequence, _, strm') => SuffixExpr_PROD_4(strm)
          | (Tok.KW_false, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.KW_true, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.OP_D, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.OP_Dotimes, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.OP_curl, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.OP_Ddot, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.LB, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.LCB, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.BAR, _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.INT(_), _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.STRING(_), _, strm') =>
              tryProds(strm, [SuffixExpr_PROD_1, SuffixExpr_PROD_2])
          | (Tok.KW_real, _, strm') => SuffixExpr_PROD_3(strm)
          | (Tok.KW_load_image, _, strm') => SuffixExpr_PROD_5(strm)
          | (Tok.KW_zeros, _, strm') => SuffixExpr_PROD_7(strm)
          | _ => fail()
        (* end case *))
      end
and Shape_NT (strm) = let
      val (LB_RES, LB_SPAN, strm') = matchLB(strm)
      fun Shape_PROD_1_SUBRULE_1_NT (strm) = let
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm)
            fun Shape_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(ConstExpr_SPAN))
                  in
                    ((ConstExpr_RES), FULL_SPAN, strm')
                  end
            fun Shape_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Shape_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, Shape_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(ConstExpr_SPAN), #2(SR_SPAN))
            in
              ((ConstExpr_RES, SR_RES), FULL_SPAN, strm')
            end
      fun Shape_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_false, _, strm') => true
              | (Tok.KW_identity, _, strm') => true
              | (Tok.KW_load_image, _, strm') => true
              | (Tok.KW_load_sequence, _, strm') => true
              | (Tok.KW_nan, _, strm') => true
              | (Tok.KW_real, _, strm') => true
              | (Tok.KW_true, _, strm') => true
              | (Tok.KW_zeros, _, strm') => true
              | (Tok.OP_minus, _, strm') => true
              | (Tok.OP_D, _, strm') => true
              | (Tok.OP_Dotimes, _, strm') => true
              | (Tok.OP_curl, _, strm') => true
              | (Tok.OP_Ddot, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.LB, _, strm') => true
              | (Tok.LCB, _, strm') => true
              | (Tok.BANG, _, strm') => true
              | (Tok.BAR, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | (Tok.INT(_), _, strm') => true
              | (Tok.REAL(_), _, strm') => true
              | (Tok.STRING(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Shape_PROD_1_SUBRULE_1_PRED, Shape_PROD_1_SUBRULE_1_NT, strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
      in
        (UserCode.Shape_PROD_1_ACT (LB_RES, RB_RES, SR_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and ConstExpr_NT (strm) = let
      val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm)
      val FULL_SPAN = (#1(Expression_SPAN), #2(Expression_SPAN))
      in
        (UserCode.ConstExpr_PROD_1_ACT (Expression_RES, Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Suffix_NT (strm) = let
      fun Suffix_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Arguments_RES, Arguments_SPAN, strm') = Arguments_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.Suffix_PROD_1_ACT (LP_RES, RP_RES, Arguments_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Suffix_PROD_2 (strm) = let
            val (LB_RES, LB_SPAN, strm') = matchLB(strm)
            val (Indices_RES, Indices_SPAN, strm') = Indices_NT(strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
            in
              (UserCode.Suffix_PROD_2_ACT (LB_RES, RB_RES, Indices_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Indices_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Suffix_PROD_3 (strm) = let
            val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(DOT_SPAN), #2(ID_SPAN))
            in
              (UserCode.Suffix_PROD_3_ACT (ID_RES, DOT_RES, ID_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DOT, _, strm') => Suffix_PROD_3(strm)
          | (Tok.LP, _, strm') => Suffix_PROD_1(strm)
          | (Tok.LB, _, strm') => Suffix_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and Indices_NT (strm) = let
      val (IndexExpr_RES, IndexExpr_SPAN, strm') = IndexExpr_NT(strm)
      fun Indices_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (IndexExpr_RES, IndexExpr_SPAN, strm') = IndexExpr_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(IndexExpr_SPAN))
            in
              ((IndexExpr_RES), FULL_SPAN, strm')
            end
      fun Indices_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Indices_PROD_1_SUBRULE_1_PRED, Indices_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(IndexExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.Indices_PROD_1_ACT (SR_RES, IndexExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), IndexExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and IndexExpr_NT (strm) = let
      fun IndexExpr_PROD_1 (strm) = let
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm)
            val FULL_SPAN = (#1(Expression_SPAN), #2(Expression_SPAN))
            in
              (UserCode.IndexExpr_PROD_1_ACT (Expression_RES, Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun IndexExpr_PROD_2 (strm) = let
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm)
            val FULL_SPAN = (#1(COLON_SPAN), #2(COLON_SPAN))
            in
              (UserCode.IndexExpr_PROD_2_ACT (COLON_RES, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.COLON, _, strm') => IndexExpr_PROD_2(strm)
          | (Tok.KW_false, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.KW_identity, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.KW_load_image, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.KW_load_sequence, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.KW_nan, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.KW_real, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.KW_true, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.KW_zeros, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.OP_minus, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.OP_D, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.OP_Dotimes, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.OP_curl, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.OP_Ddot, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.LP, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.LB, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.LCB, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.BANG, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.BAR, _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.ID(_), _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.INT(_), _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => IndexExpr_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => IndexExpr_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and Arguments_NT (strm) = let
      fun Arguments_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.Arguments_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun Arguments_PROD_2 (strm) = let
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm)
            fun Arguments_PROD_2_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Expression_SPAN))
                  in
                    ((Expression_RES), FULL_SPAN, strm')
                  end
            fun Arguments_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Arguments_PROD_2_SUBRULE_1_PRED, Arguments_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Expression_SPAN), #2(SR_SPAN))
            in
              (UserCode.Arguments_PROD_2_ACT (SR_RES, Expression_RES, SR_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_false, _, strm') => Arguments_PROD_2(strm)
          | (Tok.KW_identity, _, strm') => Arguments_PROD_2(strm)
          | (Tok.KW_load_image, _, strm') => Arguments_PROD_2(strm)
          | (Tok.KW_load_sequence, _, strm') => Arguments_PROD_2(strm)
          | (Tok.KW_nan, _, strm') => Arguments_PROD_2(strm)
          | (Tok.KW_real, _, strm') => Arguments_PROD_2(strm)
          | (Tok.KW_true, _, strm') => Arguments_PROD_2(strm)
          | (Tok.KW_zeros, _, strm') => Arguments_PROD_2(strm)
          | (Tok.OP_minus, _, strm') => Arguments_PROD_2(strm)
          | (Tok.OP_D, _, strm') => Arguments_PROD_2(strm)
          | (Tok.OP_Dotimes, _, strm') => Arguments_PROD_2(strm)
          | (Tok.OP_curl, _, strm') => Arguments_PROD_2(strm)
          | (Tok.OP_Ddot, _, strm') => Arguments_PROD_2(strm)
          | (Tok.LP, _, strm') => Arguments_PROD_2(strm)
          | (Tok.LB, _, strm') => Arguments_PROD_2(strm)
          | (Tok.LCB, _, strm') => Arguments_PROD_2(strm)
          | (Tok.BANG, _, strm') => Arguments_PROD_2(strm)
          | (Tok.BAR, _, strm') => Arguments_PROD_2(strm)
          | (Tok.ID(_), _, strm') => Arguments_PROD_2(strm)
          | (Tok.INT(_), _, strm') => Arguments_PROD_2(strm)
          | (Tok.REAL(_), _, strm') => Arguments_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => Arguments_PROD_2(strm)
          | (Tok.RP, _, strm') => Arguments_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and DiffExpr_NT (strm) = let
      fun DiffExpr_PROD_1 (strm) = let
            val (AtomicExpr_RES, AtomicExpr_SPAN, strm') = AtomicExpr_NT(strm)
            val FULL_SPAN = (#1(AtomicExpr_SPAN), #2(AtomicExpr_SPAN))
            in
              (UserCode.DiffExpr_PROD_1_ACT (AtomicExpr_RES, AtomicExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DiffExpr_PROD_2 (strm) = let
            val (DiffOp_RES, DiffOp_SPAN, strm') = DiffOp_NT(strm)
            val (DiffExpr_RES, DiffExpr_SPAN, strm') = DiffExpr_NT(strm')
            val FULL_SPAN = (#1(DiffOp_SPAN), #2(DiffExpr_SPAN))
            in
              (UserCode.DiffExpr_PROD_2_ACT (DiffExpr_RES, DiffOp_RES, DiffExpr_SPAN : (Lex.pos * Lex.pos), DiffOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_D, _, strm') => DiffExpr_PROD_2(strm)
          | (Tok.OP_Dotimes, _, strm') => DiffExpr_PROD_2(strm)
          | (Tok.OP_curl, _, strm') => DiffExpr_PROD_2(strm)
          | (Tok.OP_Ddot, _, strm') => DiffExpr_PROD_2(strm)
          | (Tok.KW_false, _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.KW_true, _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.LP, _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.LB, _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.LCB, _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.BAR, _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.ID(_), _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.INT(_), _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => DiffExpr_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => DiffExpr_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and AtomicExpr_NT (strm) = let
      fun AtomicExpr_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.AtomicExpr_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (Continuity_RES, Continuity_SPAN, strm') = Continuity_NT(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(Continuity_SPAN))
            in
              (UserCode.AtomicExpr_PROD_2_ACT (ID_RES, HASH_RES, Continuity_RES, ID_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_3 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.AtomicExpr_PROD_3_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_4 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.AtomicExpr_PROD_4_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_5 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.AtomicExpr_PROD_5_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_6 (strm) = let
            val (KW_true_RES, KW_true_SPAN, strm') = matchKW_true(strm)
            val FULL_SPAN = (#1(KW_true_SPAN), #2(KW_true_SPAN))
            in
              (UserCode.AtomicExpr_PROD_6_ACT (KW_true_RES, KW_true_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_7 (strm) = let
            val (KW_false_RES, KW_false_SPAN, strm') = matchKW_false(strm)
            val FULL_SPAN = (#1(KW_false_SPAN), #2(KW_false_SPAN))
            in
              (UserCode.AtomicExpr_PROD_7_ACT (KW_false_RES, KW_false_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_8 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicExpr_PROD_8_ACT (LP_RES, RP_RES, Expression_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_9 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.AtomicExpr_PROD_9_ACT (LCB_RES, RCB_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_10 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (SeqRest_RES, SeqRest_SPAN, strm') = SeqRest_NT(strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.AtomicExpr_PROD_10_ACT (LCB_RES, RCB_RES, Expression_RES, SeqRest_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), SeqRest_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_11 (strm) = let
            val (LB_RES, LB_SPAN, strm') = matchLB(strm)
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            fun AtomicExpr_PROD_11_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Expression_SPAN))
                  in
                    ((Expression_RES), FULL_SPAN, strm')
                  end
            fun AtomicExpr_PROD_11_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicExpr_PROD_11_SUBRULE_1_PRED, AtomicExpr_PROD_11_SUBRULE_1_NT, strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
            in
              (UserCode.AtomicExpr_PROD_11_ACT (LB_RES, RB_RES, SR_RES, Expression_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExpr_PROD_12 (strm) = let
            val (BAR1_RES, BAR1_SPAN, strm') = matchBAR(strm)
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (BAR2_RES, BAR2_SPAN, strm') = matchBAR(strm')
            val FULL_SPAN = (#1(BAR1_SPAN), #2(BAR2_SPAN))
            in
              (UserCode.AtomicExpr_PROD_12_ACT (BAR1_RES, BAR2_RES, Expression_RES, BAR1_SPAN : (Lex.pos * Lex.pos), BAR2_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.BAR, _, strm') => AtomicExpr_PROD_12(strm)
          | (Tok.LP, _, strm') => AtomicExpr_PROD_8(strm)
          | (Tok.KW_true, _, strm') => AtomicExpr_PROD_6(strm)
          | (Tok.REAL(_), _, strm') => AtomicExpr_PROD_4(strm)
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_else, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.KW_if, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_orelse, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_andalso, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_lt, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_lte, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_eqeq, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_neq, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_gte, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_gt, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_plus, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_minus, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_star, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_convolve, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_dot, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_cross, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_outer, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_slash, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_mod, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_exp, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_comp, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.OP_at, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.LP, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.RP, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.LB, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.RB, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.LCB, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.RCB, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.COMMA, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.SEMI, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.COLON, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.BAR, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.DOT, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.DOTDOT, _, strm') => AtomicExpr_PROD_1(strm)
                | (Tok.HASH, _, strm') => AtomicExpr_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.INT(_), _, strm') => AtomicExpr_PROD_3(strm)
          | (Tok.STRING(_), _, strm') => AtomicExpr_PROD_5(strm)
          | (Tok.KW_false, _, strm') => AtomicExpr_PROD_7(strm)
          | (Tok.LCB, _, strm') =>
              (case (lex(strm'))
               of (Tok.RCB, _, strm') => AtomicExpr_PROD_9(strm)
                | (Tok.KW_false, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.KW_identity, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.KW_load_image, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.KW_load_sequence, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.KW_nan, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.KW_real, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.KW_true, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.KW_zeros, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.OP_minus, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.OP_D, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.OP_Dotimes, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.OP_curl, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.OP_Ddot, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.LP, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.LB, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.LCB, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.BANG, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.BAR, _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.ID(_), _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.INT(_), _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.REAL(_), _, strm') => AtomicExpr_PROD_10(strm)
                | (Tok.STRING(_), _, strm') => AtomicExpr_PROD_10(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LB, _, strm') => AtomicExpr_PROD_11(strm)
          | _ => fail()
        (* end case *))
      end
and SeqRest_NT (strm) = let
      fun SeqRest_PROD_1 (strm) = let
            fun SeqRest_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Expression_SPAN))
                  in
                    ((Expression_RES), FULL_SPAN, strm')
                  end
            fun SeqRest_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(SeqRest_PROD_1_SUBRULE_1_PRED, SeqRest_PROD_1_SUBRULE_1_NT, strm)
            val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
            in
              (UserCode.SeqRest_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SeqRest_PROD_2 (strm) = let
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
            val (Iterator_RES, Iterator_SPAN, strm') = Iterator_NT(strm')
            val FULL_SPAN = (#1(BAR_SPAN), #2(Iterator_SPAN))
            in
              (UserCode.SeqRest_PROD_2_ACT (BAR_RES, Iterator_RES, BAR_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.BAR, _, strm') => SeqRest_PROD_2(strm)
          | (Tok.RCB, _, strm') => SeqRest_PROD_1(strm)
          | (Tok.COMMA, _, strm') => SeqRest_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Comprehension_NT (strm) = let
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
      val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
      val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm')
      val (Iterator_RES, Iterator_SPAN, strm') = Iterator_NT(strm')
      fun Comprehension_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Iterator_RES, Iterator_SPAN, strm') = Iterator_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Iterator_SPAN))
            in
              ((Iterator_RES), FULL_SPAN, strm')
            end
      fun Comprehension_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Comprehension_PROD_1_SUBRULE_1_PRED, Comprehension_PROD_1_SUBRULE_1_NT, strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
      in
        (UserCode.Comprehension_PROD_1_ACT (SR_RES, BAR_RES, LCB_RES, RCB_RES, Expression_RES, Iterator_RES, SR_SPAN : (Lex.pos * Lex.pos), BAR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun CreateStrands_NT (strm) = let
      fun CreateStrands_PROD_1 (strm) = let
            val (KW_create_collection_RES, KW_create_collection_SPAN, strm') = matchKW_create_collection(strm)
            val (Comprehension_RES, Comprehension_SPAN, strm') = Comprehension_NT(strm')
            fun CreateStrands_PROD_1_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun CreateStrands_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(CreateStrands_PROD_1_SUBRULE_1_PRED, CreateStrands_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(KW_create_collection_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.CreateStrands_PROD_1_ACT (SEMI_RES, Comprehension_RES, KW_create_collection_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Comprehension_SPAN : (Lex.pos * Lex.pos), KW_create_collection_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CreateStrands_PROD_2 (strm) = let
            val (KW_create_array_RES, KW_create_array_SPAN, strm') = matchKW_create_array(strm)
            val (Comprehension_RES, Comprehension_SPAN, strm') = Comprehension_NT(strm')
            fun CreateStrands_PROD_2_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun CreateStrands_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(CreateStrands_PROD_2_SUBRULE_1_PRED, CreateStrands_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(KW_create_array_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.CreateStrands_PROD_2_ACT (SEMI_RES, Comprehension_RES, KW_create_array_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Comprehension_SPAN : (Lex.pos * Lex.pos), KW_create_array_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_create_array, _, strm') => CreateStrands_PROD_2(strm)
          | (Tok.KW_create_collection, _, strm') => CreateStrands_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun SequenceDims_NT (strm) = let
      fun SequenceDims_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.SequenceDims_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun SequenceDims_PROD_2 (strm) = let
            val (LB_RES, LB_SPAN, strm') = matchLB(strm)
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
            in
              (UserCode.SequenceDims_PROD_2_ACT (LB_RES, RB_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SequenceDims_PROD_3 (strm) = let
            val (LB_RES, LB_SPAN, strm') = matchLB(strm)
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val (SequenceDims_RES, SequenceDims_SPAN, strm') = SequenceDims_NT(strm')
            val FULL_SPAN = (#1(LB_SPAN), #2(SequenceDims_SPAN))
            in
              (UserCode.SequenceDims_PROD_3_ACT (LB_RES, RB_RES, SequenceDims_RES, ConstExpr_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SequenceDims_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => SequenceDims_PROD_1(strm)
          | (Tok.LB, _, strm') =>
              (case (lex(strm'))
               of (Tok.RB, _, strm') => SequenceDims_PROD_2(strm)
                | (Tok.KW_false, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.KW_identity, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.KW_load_image, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.KW_load_sequence, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.KW_nan, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.KW_real, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.KW_true, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.KW_zeros, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.OP_minus, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.OP_D, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.OP_Dotimes, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.OP_curl, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.OP_Ddot, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.LP, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.LB, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.LCB, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.BANG, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.BAR, _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.ID(_), _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.INT(_), _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.REAL(_), _, strm') => SequenceDims_PROD_3(strm)
                | (Tok.STRING(_), _, strm') => SequenceDims_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun PrimitiveType_NT (strm) = let
      fun PrimitiveType_PROD_1 (strm) = let
            val (KW_tensor_RES, KW_tensor_SPAN, strm') = matchKW_tensor(strm)
            val (Shape_RES, Shape_SPAN, strm') = Shape_NT(strm')
            val FULL_SPAN = (#1(KW_tensor_SPAN), #2(Shape_SPAN))
            in
              (UserCode.PrimitiveType_PROD_1_ACT (Shape_RES, KW_tensor_RES, Shape_SPAN : (Lex.pos * Lex.pos), KW_tensor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_2 (strm) = let
            val (KW_vec2_RES, KW_vec2_SPAN, strm') = matchKW_vec2(strm)
            val FULL_SPAN = (#1(KW_vec2_SPAN), #2(KW_vec2_SPAN))
            in
              (UserCode.PrimitiveType_PROD_2_ACT (KW_vec2_RES, KW_vec2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_3 (strm) = let
            val (KW_vec3_RES, KW_vec3_SPAN, strm') = matchKW_vec3(strm)
            val FULL_SPAN = (#1(KW_vec3_SPAN), #2(KW_vec3_SPAN))
            in
              (UserCode.PrimitiveType_PROD_3_ACT (KW_vec3_RES, KW_vec3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_4 (strm) = let
            val (KW_vec4_RES, KW_vec4_SPAN, strm') = matchKW_vec4(strm)
            val FULL_SPAN = (#1(KW_vec4_SPAN), #2(KW_vec4_SPAN))
            in
              (UserCode.PrimitiveType_PROD_4_ACT (KW_vec4_RES, KW_vec4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_5 (strm) = let
            val (KW_mat2_RES, KW_mat2_SPAN, strm') = matchKW_mat2(strm)
            val FULL_SPAN = (#1(KW_mat2_SPAN), #2(KW_mat2_SPAN))
            in
              (UserCode.PrimitiveType_PROD_5_ACT (KW_mat2_RES, KW_mat2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_6 (strm) = let
            val (KW_mat3_RES, KW_mat3_SPAN, strm') = matchKW_mat3(strm)
            val FULL_SPAN = (#1(KW_mat3_SPAN), #2(KW_mat3_SPAN))
            in
              (UserCode.PrimitiveType_PROD_6_ACT (KW_mat3_RES, KW_mat3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_7 (strm) = let
            val (KW_mat4_RES, KW_mat4_SPAN, strm') = matchKW_mat4(strm)
            val FULL_SPAN = (#1(KW_mat4_SPAN), #2(KW_mat4_SPAN))
            in
              (UserCode.PrimitiveType_PROD_7_ACT (KW_mat4_RES, KW_mat4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_8 (strm) = let
            val (KW_bool_RES, KW_bool_SPAN, strm') = matchKW_bool(strm)
            val FULL_SPAN = (#1(KW_bool_SPAN), #2(KW_bool_SPAN))
            in
              (UserCode.PrimitiveType_PROD_8_ACT (KW_bool_RES, KW_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_9 (strm) = let
            val (KW_int_RES, KW_int_SPAN, strm') = matchKW_int(strm)
            val FULL_SPAN = (#1(KW_int_SPAN), #2(KW_int_SPAN))
            in
              (UserCode.PrimitiveType_PROD_9_ACT (KW_int_RES, KW_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_10 (strm) = let
            val (KW_real_RES, KW_real_SPAN, strm') = matchKW_real(strm)
            val FULL_SPAN = (#1(KW_real_SPAN), #2(KW_real_SPAN))
            in
              (UserCode.PrimitiveType_PROD_10_ACT (KW_real_RES, KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_11 (strm) = let
            val (KW_string_RES, KW_string_SPAN, strm') = matchKW_string(strm)
            val FULL_SPAN = (#1(KW_string_SPAN), #2(KW_string_SPAN))
            in
              (UserCode.PrimitiveType_PROD_11_ACT (KW_string_RES, KW_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimitiveType_PROD_12 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.PrimitiveType_PROD_12_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => PrimitiveType_PROD_12(strm)
          | (Tok.KW_real, _, strm') => PrimitiveType_PROD_10(strm)
          | (Tok.KW_bool, _, strm') => PrimitiveType_PROD_8(strm)
          | (Tok.KW_mat3, _, strm') => PrimitiveType_PROD_6(strm)
          | (Tok.KW_vec4, _, strm') => PrimitiveType_PROD_4(strm)
          | (Tok.KW_vec2, _, strm') => PrimitiveType_PROD_2(strm)
          | (Tok.KW_tensor, _, strm') => PrimitiveType_PROD_1(strm)
          | (Tok.KW_vec3, _, strm') => PrimitiveType_PROD_3(strm)
          | (Tok.KW_mat2, _, strm') => PrimitiveType_PROD_5(strm)
          | (Tok.KW_mat4, _, strm') => PrimitiveType_PROD_7(strm)
          | (Tok.KW_int, _, strm') => PrimitiveType_PROD_9(strm)
          | (Tok.KW_string, _, strm') => PrimitiveType_PROD_11(strm)
          | _ => fail()
        (* end case *))
      end
fun ConcreteType_NT (strm) = let
      val (PrimitiveType_RES, PrimitiveType_SPAN, strm') = PrimitiveType_NT(strm)
      val (SequenceDims_RES, SequenceDims_SPAN, strm') = SequenceDims_NT(strm')
      val FULL_SPAN = (#1(PrimitiveType_SPAN), #2(SequenceDims_SPAN))
      in
        (UserCode.ConcreteType_PROD_1_ACT (SequenceDims_RES, PrimitiveType_RES, SequenceDims_SPAN : (Lex.pos * Lex.pos), PrimitiveType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Type_NT (strm) = let
      fun Type_PROD_1 (strm) = let
            val (KW_image_RES, KW_image_SPAN, strm') = matchKW_image(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Shape_RES, Shape_SPAN, strm') = Shape_NT(strm')
            val FULL_SPAN = (#1(KW_image_SPAN), #2(Shape_SPAN))
            in
              (UserCode.Type_PROD_1_ACT (LP_RES, RP_RES, KW_image_RES, Shape_RES, ConstExpr_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_2 (strm) = let
            val (KW_field_RES, KW_field_SPAN, strm') = matchKW_field(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (Continuity_RES, Continuity_SPAN, strm') = Continuity_NT(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Shape_RES, Shape_SPAN, strm') = Shape_NT(strm')
            val FULL_SPAN = (#1(KW_field_SPAN), #2(Shape_SPAN))
            in
              (UserCode.Type_PROD_2_ACT (LP_RES, RP_RES, HASH_RES, KW_field_RES, Continuity_RES, Shape_RES, ConstExpr_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_field_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_3 (strm) = let
            val (KW_OField_RES, KW_OField_SPAN, strm') = matchKW_OField(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (Continuity_RES, Continuity_SPAN, strm') = Continuity_NT(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Shape_RES, Shape_SPAN, strm') = Shape_NT(strm')
            val FULL_SPAN = (#1(KW_OField_SPAN), #2(Shape_SPAN))
            in
              (UserCode.Type_PROD_3_ACT (LP_RES, RP_RES, HASH_RES, KW_OField_RES, Continuity_RES, Shape_RES, ConstExpr_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_OField_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_4 (strm) = let
            val (KW_fem_RES, KW_fem_SPAN, strm') = matchKW_fem(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (Continuity_RES, Continuity_SPAN, strm') = Continuity_NT(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Shape_RES, Shape_SPAN, strm') = Shape_NT(strm')
            val FULL_SPAN = (#1(KW_fem_SPAN), #2(Shape_SPAN))
            in
              (UserCode.Type_PROD_4_ACT (LP_RES, RP_RES, HASH_RES, Continuity_RES, Shape_RES, ConstExpr_RES, KW_fem_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), Shape_SPAN : (Lex.pos * Lex.pos), ConstExpr_SPAN : (Lex.pos * Lex.pos), KW_fem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_5 (strm) = let
            val (KW_mesh_RES, KW_mesh_SPAN, strm') = matchKW_mesh(strm)
            val FULL_SPAN = (#1(KW_mesh_SPAN), #2(KW_mesh_SPAN))
            in
              (UserCode.Type_PROD_5_ACT (KW_mesh_RES, KW_mesh_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_6 (strm) = let
            val (KW_element_RES, KW_element_SPAN, strm') = matchKW_element(strm)
            val FULL_SPAN = (#1(KW_element_SPAN), #2(KW_element_SPAN))
            in
              (UserCode.Type_PROD_6_ACT (KW_element_RES, KW_element_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_7 (strm) = let
            val (KW_fnspace_RES, KW_fnspace_SPAN, strm') = matchKW_fnspace(strm)
            val FULL_SPAN = (#1(KW_fnspace_SPAN), #2(KW_fnspace_SPAN))
            in
              (UserCode.Type_PROD_7_ACT (KW_fnspace_RES, KW_fnspace_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_8 (strm) = let
            val (KW_kernel_RES, KW_kernel_SPAN, strm') = matchKW_kernel(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (Continuity_RES, Continuity_SPAN, strm') = Continuity_NT(strm')
            val FULL_SPAN = (#1(KW_kernel_SPAN), #2(Continuity_SPAN))
            in
              (UserCode.Type_PROD_8_ACT (HASH_RES, Continuity_RES, KW_kernel_RES, HASH_SPAN : (Lex.pos * Lex.pos), Continuity_SPAN : (Lex.pos * Lex.pos), KW_kernel_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_9 (strm) = let
            val (ConcreteType_RES, ConcreteType_SPAN, strm') = ConcreteType_NT(strm)
            val FULL_SPAN = (#1(ConcreteType_SPAN), #2(ConcreteType_SPAN))
            in
              (UserCode.Type_PROD_9_ACT (ConcreteType_RES, ConcreteType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_bool, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_int, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_mat2, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_mat3, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_mat4, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_real, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_string, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_tensor, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_vec2, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_vec3, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_vec4, _, strm') => Type_PROD_9(strm)
          | (Tok.ID(_), _, strm') => Type_PROD_9(strm)
          | (Tok.KW_fnspace, _, strm') => Type_PROD_7(strm)
          | (Tok.KW_mesh, _, strm') => Type_PROD_5(strm)
          | (Tok.KW_OField, _, strm') => Type_PROD_3(strm)
          | (Tok.KW_image, _, strm') => Type_PROD_1(strm)
          | (Tok.KW_field, _, strm') => Type_PROD_2(strm)
          | (Tok.KW_fem, _, strm') => Type_PROD_4(strm)
          | (Tok.KW_element, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_kernel, _, strm') => Type_PROD_8(strm)
          | _ => fail()
        (* end case *))
      end
fun AssignOp_NT (strm) = let
      fun AssignOp_PROD_1 (strm) = let
            val (OP_pluseq_RES, OP_pluseq_SPAN, strm') = matchOP_pluseq(strm)
            val FULL_SPAN = (#1(OP_pluseq_SPAN), #2(OP_pluseq_SPAN))
            in
              (UserCode.AssignOp_PROD_1_ACT (OP_pluseq_RES, OP_pluseq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AssignOp_PROD_2 (strm) = let
            val (OP_minuseq_RES, OP_minuseq_SPAN, strm') = matchOP_minuseq(strm)
            val FULL_SPAN = (#1(OP_minuseq_SPAN), #2(OP_minuseq_SPAN))
            in
              (UserCode.AssignOp_PROD_2_ACT (OP_minuseq_RES, OP_minuseq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AssignOp_PROD_3 (strm) = let
            val (OP_stareq_RES, OP_stareq_SPAN, strm') = matchOP_stareq(strm)
            val FULL_SPAN = (#1(OP_stareq_SPAN), #2(OP_stareq_SPAN))
            in
              (UserCode.AssignOp_PROD_3_ACT (OP_stareq_RES, OP_stareq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AssignOp_PROD_4 (strm) = let
            val (OP_slasheq_RES, OP_slasheq_SPAN, strm') = matchOP_slasheq(strm)
            val FULL_SPAN = (#1(OP_slasheq_SPAN), #2(OP_slasheq_SPAN))
            in
              (UserCode.AssignOp_PROD_4_ACT (OP_slasheq_RES, OP_slasheq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AssignOp_PROD_5 (strm) = let
            val (OP_modeq_RES, OP_modeq_SPAN, strm') = matchOP_modeq(strm)
            val FULL_SPAN = (#1(OP_modeq_SPAN), #2(OP_modeq_SPAN))
            in
              (UserCode.AssignOp_PROD_5_ACT (OP_modeq_RES, OP_modeq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_modeq, _, strm') => AssignOp_PROD_5(strm)
          | (Tok.OP_stareq, _, strm') => AssignOp_PROD_3(strm)
          | (Tok.OP_pluseq, _, strm') => AssignOp_PROD_1(strm)
          | (Tok.OP_minuseq, _, strm') => AssignOp_PROD_2(strm)
          | (Tok.OP_slasheq, _, strm') => AssignOp_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
fun VarDcl_NT (strm) = let
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm)
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      fun VarDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm)
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val FULL_SPAN = (#1(OP_eq_SPAN), #2(Expression_SPAN))
            in
              ((Expression_RES), FULL_SPAN, strm')
            end
      fun VarDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_eq, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(VarDcl_PROD_1_SUBRULE_1_PRED, VarDcl_PROD_1_SUBRULE_1_NT, strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(Type_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.VarDcl_PROD_1_ACT (SR_RES, SEMI_RES, Type_RES, BindId_RES, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Block_NT (strm) = let
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
      fun Block_PROD_1_SUBRULE_1_NT (strm) = let
            val (Statement_RES, Statement_SPAN, strm') = Statement_NT(strm)
            val FULL_SPAN = (#1(Statement_SPAN), #2(Statement_SPAN))
            in
              ((Statement_RES), FULL_SPAN, strm')
            end
      fun Block_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_bool, _, strm') => true
              | (Tok.KW_continue, _, strm') => true
              | (Tok.KW_die, _, strm') => true
              | (Tok.KW_field, _, strm') => true
              | (Tok.KW_foreach, _, strm') => true
              | (Tok.KW_if, _, strm') => true
              | (Tok.KW_image, _, strm') => true
              | (Tok.KW_int, _, strm') => true
              | (Tok.KW_kernel, _, strm') => true
              | (Tok.KW_mat2, _, strm') => true
              | (Tok.KW_mat3, _, strm') => true
              | (Tok.KW_mat4, _, strm') => true
              | (Tok.KW_new, _, strm') => true
              | (Tok.KW_print, _, strm') => true
              | (Tok.KW_real, _, strm') => true
              | (Tok.KW_return, _, strm') => true
              | (Tok.KW_stabilize, _, strm') => true
              | (Tok.KW_string, _, strm') => true
              | (Tok.KW_tensor, _, strm') => true
              | (Tok.KW_vec2, _, strm') => true
              | (Tok.KW_vec3, _, strm') => true
              | (Tok.KW_vec4, _, strm') => true
              | (Tok.KW_OField, _, strm') => true
              | (Tok.KW_fem, _, strm') => true
              | (Tok.KW_mesh, _, strm') => true
              | (Tok.KW_element, _, strm') => true
              | (Tok.KW_fnspace, _, strm') => true
              | (Tok.LCB, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Block_PROD_1_SUBRULE_1_PRED, Block_PROD_1_SUBRULE_1_NT, strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
      in
        (UserCode.Block_PROD_1_ACT (SR_RES, LCB_RES, RCB_RES, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Statement_NT (strm) = let
      fun Statement_PROD_1 (strm) = let
            val (AtomicStmt_RES, AtomicStmt_SPAN, strm') = AtomicStmt_NT(strm)
            val FULL_SPAN = (#1(AtomicStmt_SPAN), #2(AtomicStmt_SPAN))
            in
              ((AtomicStmt_RES), FULL_SPAN, strm')
            end
      fun Statement_PROD_2 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Statement_RES, Statement_SPAN, strm') = Statement_NT(strm')
            val (IfRest_RES, IfRest_SPAN, strm') = IfRest_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(IfRest_SPAN))
            in
              (UserCode.Statement_PROD_2_ACT (LP_RES, RP_RES, Expression_RES, KW_if_RES, Statement_RES, IfRest_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), Statement_SPAN : (Lex.pos * Lex.pos), IfRest_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Statement_PROD_3 (strm) = let
            val (KW_foreach_RES, KW_foreach_SPAN, strm') = matchKW_foreach(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val (Iterator_RES, Iterator_SPAN, strm') = Iterator_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Block_RES, Block_SPAN, strm') = Block_NT(strm')
            val FULL_SPAN = (#1(KW_foreach_SPAN), #2(Block_SPAN))
            in
              (UserCode.Statement_PROD_3_ACT (LP_RES, RP_RES, Type_RES, Block_RES, KW_foreach_RES, Iterator_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), KW_foreach_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_foreach, _, strm') => Statement_PROD_3(strm)
          | (Tok.KW_bool, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_continue, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_die, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_field, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_image, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_int, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_kernel, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_mat2, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_mat3, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_mat4, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_new, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_print, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_real, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_return, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_stabilize, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_string, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_tensor, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_vec2, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_vec3, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_vec4, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_OField, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_fem, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_mesh, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_element, _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_fnspace, _, strm') => Statement_PROD_1(strm)
          | (Tok.LCB, _, strm') => Statement_PROD_1(strm)
          | (Tok.ID(_), _, strm') => Statement_PROD_1(strm)
          | (Tok.KW_if, _, strm') => Statement_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and IfRest_NT (strm) = let
      fun IfRest_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.IfRest_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun IfRest_PROD_2 (strm) = let
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm)
            val (Statement_RES, Statement_SPAN, strm') = Statement_NT(strm')
            val FULL_SPAN = (#1(KW_else_SPAN), #2(Statement_SPAN))
            in
              (UserCode.IfRest_PROD_2_ACT (KW_else_RES, Statement_RES, KW_else_SPAN : (Lex.pos * Lex.pos), Statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_else, _, strm') => IfRest_PROD_2(strm)
          | (Tok.KW_bool, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_continue, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_die, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_field, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_foreach, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_if, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_image, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_int, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_kernel, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_mat2, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_mat3, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_mat4, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_new, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_print, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_real, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_return, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_stabilize, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_string, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_tensor, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_vec2, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_vec3, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_vec4, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_OField, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_fem, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_mesh, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_element, _, strm') => IfRest_PROD_1(strm)
          | (Tok.KW_fnspace, _, strm') => IfRest_PROD_1(strm)
          | (Tok.LCB, _, strm') => IfRest_PROD_1(strm)
          | (Tok.RCB, _, strm') => IfRest_PROD_1(strm)
          | (Tok.ID(_), _, strm') => IfRest_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and AtomicStmt_NT (strm) = let
      fun AtomicStmt_PROD_1 (strm) = let
            val (Block_RES, Block_SPAN, strm') = Block_NT(strm)
            val FULL_SPAN = (#1(Block_SPAN), #2(Block_SPAN))
            in
              (UserCode.AtomicStmt_PROD_1_ACT (Block_RES, Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_2 (strm) = let
            val (KW_print_RES, KW_print_SPAN, strm') = matchKW_print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Arguments_RES, Arguments_SPAN, strm') = Arguments_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_print_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_2_ACT (LP_RES, RP_RES, SEMI_RES, KW_print_RES, Arguments_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_print_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_3 (strm) = let
            val (KW_new_RES, KW_new_SPAN, strm') = matchKW_new(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Arguments_RES, Arguments_SPAN, strm') = Arguments_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_new_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_3_ACT (ID_RES, LP_RES, RP_RES, SEMI_RES, Arguments_RES, KW_new_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), KW_new_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_4 (strm) = let
            val (KW_stabilize_RES, KW_stabilize_SPAN, strm') = matchKW_stabilize(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_stabilize_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_4_ACT (SEMI_RES, KW_stabilize_RES, SEMI_SPAN : (Lex.pos * Lex.pos), KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_5 (strm) = let
            val (KW_die_RES, KW_die_SPAN, strm') = matchKW_die(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_die_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_5_ACT (SEMI_RES, KW_die_RES, SEMI_SPAN : (Lex.pos * Lex.pos), KW_die_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_6 (strm) = let
            val (KW_continue_RES, KW_continue_SPAN, strm') = matchKW_continue(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_continue_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_6_ACT (SEMI_RES, KW_continue_RES, SEMI_SPAN : (Lex.pos * Lex.pos), KW_continue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_7 (strm) = let
            val (KW_return_RES, KW_return_SPAN, strm') = matchKW_return(strm)
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_return_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_7_ACT (SEMI_RES, Expression_RES, KW_return_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), KW_return_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_8 (strm) = let
            val (VarDcl_RES, VarDcl_SPAN, strm') = VarDcl_NT(strm)
            val FULL_SPAN = (#1(VarDcl_SPAN), #2(VarDcl_SPAN))
            in
              (UserCode.AtomicStmt_PROD_8_ACT (VarDcl_RES, VarDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_9 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm')
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_9_ACT (SEMI_RES, Expression_RES, OP_eq_RES, BindId_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_10 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (AssignOp_RES, AssignOp_SPAN, strm') = AssignOp_NT(strm')
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_10_ACT (SEMI_RES, Expression_RES, AssignOp_RES, BindId_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), AssignOp_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_bool, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_field, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_image, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_int, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_kernel, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_mat2, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_mat3, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_mat4, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_real, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_string, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_tensor, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_vec2, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_vec3, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_vec4, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_OField, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_fem, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_mesh, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_element, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.KW_fnspace, _, strm') => AtomicStmt_PROD_8(strm)
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.LB, _, strm') => AtomicStmt_PROD_8(strm)
                | (Tok.ID(_), _, strm') => AtomicStmt_PROD_8(strm)
                | (Tok.OP_pluseq, _, strm') => AtomicStmt_PROD_10(strm)
                | (Tok.OP_minuseq, _, strm') => AtomicStmt_PROD_10(strm)
                | (Tok.OP_stareq, _, strm') => AtomicStmt_PROD_10(strm)
                | (Tok.OP_slasheq, _, strm') => AtomicStmt_PROD_10(strm)
                | (Tok.OP_modeq, _, strm') => AtomicStmt_PROD_10(strm)
                | (Tok.OP_eq, _, strm') => AtomicStmt_PROD_9(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_continue, _, strm') => AtomicStmt_PROD_6(strm)
          | (Tok.KW_stabilize, _, strm') => AtomicStmt_PROD_4(strm)
          | (Tok.KW_print, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.LCB, _, strm') => AtomicStmt_PROD_1(strm)
          | (Tok.KW_new, _, strm') => AtomicStmt_PROD_3(strm)
          | (Tok.KW_die, _, strm') => AtomicStmt_PROD_5(strm)
          | (Tok.KW_return, _, strm') => AtomicStmt_PROD_7(strm)
          | _ => fail()
        (* end case *))
      end
fun GlobalUpdate_NT (strm) = let
      val (KW_update_RES, KW_update_SPAN, strm') = matchKW_update(strm)
      val (Block_RES, Block_SPAN, strm') = Block_NT(strm')
      fun GlobalUpdate_PROD_1_SUBRULE_1_NT (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun GlobalUpdate_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.SEMI, _, strm') => true
              | _ => false
            (* end case *))
      val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(GlobalUpdate_PROD_1_SUBRULE_1_PRED, GlobalUpdate_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(KW_update_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.GlobalUpdate_PROD_1_ACT (SEMI_RES, Block_RES, KW_update_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), KW_update_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun GlobalStart_NT (strm) = let
      val (KW_start_RES, KW_start_SPAN, strm') = matchKW_start(strm)
      val (Block_RES, Block_SPAN, strm') = Block_NT(strm')
      fun GlobalStart_PROD_1_SUBRULE_1_NT (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun GlobalStart_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.SEMI, _, strm') => true
              | _ => false
            (* end case *))
      val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(GlobalStart_PROD_1_SUBRULE_1_PRED, GlobalStart_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(KW_start_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.GlobalStart_PROD_1_ACT (SEMI_RES, KW_start_RES, Block_RES, SEMI_SPAN : (Lex.pos * Lex.pos), KW_start_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun MethodName_NT (strm) = let
      fun MethodName_PROD_1 (strm) = let
            val (KW_start_RES, KW_start_SPAN, strm') = matchKW_start(strm)
            val FULL_SPAN = (#1(KW_start_SPAN), #2(KW_start_SPAN))
            in
              (UserCode.MethodName_PROD_1_ACT (KW_start_RES, KW_start_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MethodName_PROD_2 (strm) = let
            val (KW_update_RES, KW_update_SPAN, strm') = matchKW_update(strm)
            val FULL_SPAN = (#1(KW_update_SPAN), #2(KW_update_SPAN))
            in
              (UserCode.MethodName_PROD_2_ACT (KW_update_RES, KW_update_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MethodName_PROD_3 (strm) = let
            val (KW_stabilize_RES, KW_stabilize_SPAN, strm') = matchKW_stabilize(strm)
            val FULL_SPAN = (#1(KW_stabilize_SPAN), #2(KW_stabilize_SPAN))
            in
              (UserCode.MethodName_PROD_3_ACT (KW_stabilize_RES, KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_stabilize, _, strm') => MethodName_PROD_3(strm)
          | (Tok.KW_start, _, strm') => MethodName_PROD_1(strm)
          | (Tok.KW_update, _, strm') => MethodName_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun MethodDcl_NT (strm) = let
      val (MethodName_RES, MethodName_SPAN, strm') = MethodName_NT(strm)
      val (Block_RES, Block_SPAN, strm') = Block_NT(strm')
      fun MethodDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun MethodDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.SEMI, _, strm') => true
              | _ => false
            (* end case *))
      val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(MethodDcl_PROD_1_SUBRULE_1_PRED, MethodDcl_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(MethodName_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.MethodDcl_PROD_1_ACT (SEMI_RES, Block_RES, MethodName_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), MethodName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun InitializeBlock_NT (strm) = let
      val (KW_initialize_RES, KW_initialize_SPAN, strm') = matchKW_initialize(strm)
      val (Block_RES, Block_SPAN, strm') = Block_NT(strm')
      fun InitializeBlock_PROD_1_SUBRULE_1_NT (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun InitializeBlock_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.SEMI, _, strm') => true
              | _ => false
            (* end case *))
      val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(InitializeBlock_PROD_1_SUBRULE_1_PRED, InitializeBlock_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(KW_initialize_SPAN), #2(SEMI_SPAN))
      in
        ((Block_RES), FULL_SPAN, strm')
      end
fun StateVarDcl_NT (strm) = let
      fun StateVarDcl_PROD_1 (strm) = let
            val (KW_output_RES, KW_output_SPAN, strm') = matchKW_output(strm)
            val (VarDcl_RES, VarDcl_SPAN, strm') = VarDcl_NT(strm')
            val FULL_SPAN = (#1(KW_output_SPAN), #2(VarDcl_SPAN))
            in
              (UserCode.StateVarDcl_PROD_1_ACT (VarDcl_RES, KW_output_RES, VarDcl_SPAN : (Lex.pos * Lex.pos), KW_output_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun StateVarDcl_PROD_2 (strm) = let
            val (VarDcl_RES, VarDcl_SPAN, strm') = VarDcl_NT(strm)
            val FULL_SPAN = (#1(VarDcl_SPAN), #2(VarDcl_SPAN))
            in
              (UserCode.StateVarDcl_PROD_2_ACT (VarDcl_RES, VarDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_bool, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_field, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_image, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_int, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_kernel, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_mat2, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_mat3, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_mat4, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_real, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_string, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_tensor, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_vec2, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_vec3, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_vec4, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_OField, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_fem, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_mesh, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_element, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_fnspace, _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.ID(_), _, strm') => StateVarDcl_PROD_2(strm)
          | (Tok.KW_output, _, strm') => StateVarDcl_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Parameter_NT (strm) = let
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm)
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      val FULL_SPAN = (#1(Type_SPAN), #2(BindId_SPAN))
      in
        (UserCode.Parameter_PROD_1_ACT (Type_RES, BindId_RES, Type_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Parameters_NT (strm) = let
      fun Parameters_PROD_1_SUBRULE_1_NT (strm) = let
            val (Parameter_RES, Parameter_SPAN, strm') = Parameter_NT(strm)
            fun Parameters_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Parameter_RES, Parameter_SPAN, strm') = Parameter_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Parameter_SPAN))
                  in
                    ((Parameter_RES), FULL_SPAN, strm')
                  end
            fun Parameters_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Parameters_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, Parameters_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Parameter_SPAN), #2(SR_SPAN))
            in
              ((Parameter_RES, SR_RES), FULL_SPAN, strm')
            end
      fun Parameters_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_bool, _, strm') => true
              | (Tok.KW_field, _, strm') => true
              | (Tok.KW_image, _, strm') => true
              | (Tok.KW_int, _, strm') => true
              | (Tok.KW_kernel, _, strm') => true
              | (Tok.KW_mat2, _, strm') => true
              | (Tok.KW_mat3, _, strm') => true
              | (Tok.KW_mat4, _, strm') => true
              | (Tok.KW_real, _, strm') => true
              | (Tok.KW_string, _, strm') => true
              | (Tok.KW_tensor, _, strm') => true
              | (Tok.KW_vec2, _, strm') => true
              | (Tok.KW_vec3, _, strm') => true
              | (Tok.KW_vec4, _, strm') => true
              | (Tok.KW_OField, _, strm') => true
              | (Tok.KW_fem, _, strm') => true
              | (Tok.KW_mesh, _, strm') => true
              | (Tok.KW_element, _, strm') => true
              | (Tok.KW_fnspace, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Parameters_PROD_1_SUBRULE_1_PRED, Parameters_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.Parameters_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun StrandDcl_NT (strm) = let
      val (KW_strand_RES, KW_strand_SPAN, strm') = matchKW_strand(strm)
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (Parameters_RES, Parameters_SPAN, strm') = Parameters_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm')
      fun StrandDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (StateVarDcl_RES, StateVarDcl_SPAN, strm') = StateVarDcl_NT(strm)
            val FULL_SPAN = (#1(StateVarDcl_SPAN), #2(StateVarDcl_SPAN))
            in
              ((StateVarDcl_RES), FULL_SPAN, strm')
            end
      fun StrandDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_bool, _, strm') => true
              | (Tok.KW_field, _, strm') => true
              | (Tok.KW_image, _, strm') => true
              | (Tok.KW_int, _, strm') => true
              | (Tok.KW_kernel, _, strm') => true
              | (Tok.KW_mat2, _, strm') => true
              | (Tok.KW_mat3, _, strm') => true
              | (Tok.KW_mat4, _, strm') => true
              | (Tok.KW_output, _, strm') => true
              | (Tok.KW_real, _, strm') => true
              | (Tok.KW_string, _, strm') => true
              | (Tok.KW_tensor, _, strm') => true
              | (Tok.KW_vec2, _, strm') => true
              | (Tok.KW_vec3, _, strm') => true
              | (Tok.KW_vec4, _, strm') => true
              | (Tok.KW_OField, _, strm') => true
              | (Tok.KW_fem, _, strm') => true
              | (Tok.KW_mesh, _, strm') => true
              | (Tok.KW_element, _, strm') => true
              | (Tok.KW_fnspace, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (StateVarDcl_RES, StateVarDcl_SPAN, strm') = EBNF.closure(StrandDcl_PROD_1_SUBRULE_1_PRED, StrandDcl_PROD_1_SUBRULE_1_NT, strm')
      fun StrandDcl_PROD_1_SUBRULE_2_NT (strm) = let
            val (InitializeBlock_RES, InitializeBlock_SPAN, strm') = InitializeBlock_NT(strm)
            val FULL_SPAN = (#1(InitializeBlock_SPAN),
              #2(InitializeBlock_SPAN))
            in
              ((InitializeBlock_RES), FULL_SPAN, strm')
            end
      fun StrandDcl_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_initialize, _, strm') => true
              | _ => false
            (* end case *))
      val (InitializeBlock_RES, InitializeBlock_SPAN, strm') = EBNF.optional(StrandDcl_PROD_1_SUBRULE_2_PRED, StrandDcl_PROD_1_SUBRULE_2_NT, strm')
      fun StrandDcl_PROD_1_SUBRULE_3_NT (strm) = let
            val (MethodDcl_RES, MethodDcl_SPAN, strm') = MethodDcl_NT(strm)
            val FULL_SPAN = (#1(MethodDcl_SPAN), #2(MethodDcl_SPAN))
            in
              ((MethodDcl_RES), FULL_SPAN, strm')
            end
      fun StrandDcl_PROD_1_SUBRULE_3_PRED (strm) = (case (lex(strm))
             of (Tok.KW_stabilize, _, strm') => true
              | (Tok.KW_start, _, strm') => true
              | (Tok.KW_update, _, strm') => true
              | _ => false
            (* end case *))
      val (MethodDcl_RES, MethodDcl_SPAN, strm') = EBNF.closure(StrandDcl_PROD_1_SUBRULE_3_PRED, StrandDcl_PROD_1_SUBRULE_3_NT, strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      fun StrandDcl_PROD_1_SUBRULE_4_NT (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun StrandDcl_PROD_1_SUBRULE_4_PRED (strm) = (case (lex(strm))
             of (Tok.SEMI, _, strm') => true
              | _ => false
            (* end case *))
      val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(StrandDcl_PROD_1_SUBRULE_4_PRED, StrandDcl_PROD_1_SUBRULE_4_NT, strm')
      val FULL_SPAN = (#1(KW_strand_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.StrandDcl_PROD_1_ACT (LP_RES, RP_RES, LCB_RES, RCB_RES, SEMI_RES, Parameters_RES, InitializeBlock_RES, StateVarDcl_RES, BindId_RES, KW_strand_RES, MethodDcl_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Parameters_SPAN : (Lex.pos * Lex.pos), InitializeBlock_SPAN : (Lex.pos * Lex.pos), StateVarDcl_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), KW_strand_SPAN : (Lex.pos * Lex.pos), MethodDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun FunctionDef_NT (strm) = let
      fun FunctionDef_PROD_1 (strm) = let
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm)
            val (Expression_RES, Expression_SPAN, strm') = Expression_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(OP_eq_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.FunctionDef_PROD_1_ACT (SEMI_RES, Expression_RES, OP_eq_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Expression_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun FunctionDef_PROD_2 (strm) = let
            val (Block_RES, Block_SPAN, strm') = Block_NT(strm)
            fun FunctionDef_PROD_2_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun FunctionDef_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SEMI_RES, SEMI_SPAN, strm') = EBNF.optional(FunctionDef_PROD_2_SUBRULE_1_PRED, FunctionDef_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Block_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.FunctionDef_PROD_2_ACT (SEMI_RES, Block_RES, SEMI_SPAN : (Lex.pos * Lex.pos), Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LCB, _, strm') => FunctionDef_PROD_2(strm)
          | (Tok.OP_eq, _, strm') => FunctionDef_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun FunctionDcl_NT (strm) = let
      val (KW_function_RES, KW_function_SPAN, strm') = matchKW_function(strm)
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (Parameters_RES, Parameters_SPAN, strm') = Parameters_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val (FunctionDef_RES, FunctionDef_SPAN, strm') = FunctionDef_NT(strm')
      val FULL_SPAN = (#1(KW_function_SPAN), #2(FunctionDef_SPAN))
      in
        (UserCode.FunctionDcl_PROD_1_ACT (LP_RES, RP_RES, Type_RES, Parameters_RES, KW_function_RES, BindId_RES, FunctionDef_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), Parameters_SPAN : (Lex.pos * Lex.pos), KW_function_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FunctionDef_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ConstDcl_NT (strm) = let
      val (KW_const_RES, KW_const_SPAN, strm') = matchKW_const(strm)
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      fun ConstDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm)
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val FULL_SPAN = (#1(OP_eq_SPAN), #2(ConstExpr_SPAN))
            in
              ((ConstExpr_RES), FULL_SPAN, strm')
            end
      fun ConstDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_eq, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(ConstDcl_PROD_1_SUBRULE_1_PRED, ConstDcl_PROD_1_SUBRULE_1_NT, strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(KW_const_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.ConstDcl_PROD_1_ACT (SR_RES, SEMI_RES, Type_RES, KW_const_RES, BindId_RES, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_const_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun InputDcl_NT (strm) = let
      val (KW_input_RES, KW_input_SPAN, strm') = matchKW_input(strm)
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      fun InputDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((STRING_RES), FULL_SPAN, strm')
            end
      fun InputDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LP, _, strm') => true
              | _ => false
            (* end case *))
      val (SR1_RES, SR1_SPAN, strm') = EBNF.optional(InputDcl_PROD_1_SUBRULE_1_PRED, InputDcl_PROD_1_SUBRULE_1_NT, strm')
      fun InputDcl_PROD_1_SUBRULE_2_NT (strm) = let
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm)
            val (ConstExpr_RES, ConstExpr_SPAN, strm') = ConstExpr_NT(strm')
            val FULL_SPAN = (#1(OP_eq_SPAN), #2(ConstExpr_SPAN))
            in
              ((ConstExpr_RES), FULL_SPAN, strm')
            end
      fun InputDcl_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.OP_eq, _, strm') => true
              | _ => false
            (* end case *))
      val (SR2_RES, SR2_SPAN, strm') = EBNF.optional(InputDcl_PROD_1_SUBRULE_2_PRED, InputDcl_PROD_1_SUBRULE_2_NT, strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(KW_input_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.InputDcl_PROD_1_ACT (SR1_RES, SR2_RES, SEMI_RES, Type_RES, KW_input_RES, BindId_RES, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_input_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun GlobalDcl_NT (strm) = let
      fun GlobalDcl_PROD_1 (strm) = let
            val (InputDcl_RES, InputDcl_SPAN, strm') = InputDcl_NT(strm)
            val FULL_SPAN = (#1(InputDcl_SPAN), #2(InputDcl_SPAN))
            in
              (UserCode.GlobalDcl_PROD_1_ACT (InputDcl_RES, InputDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun GlobalDcl_PROD_2 (strm) = let
            val (ConstDcl_RES, ConstDcl_SPAN, strm') = ConstDcl_NT(strm)
            val FULL_SPAN = (#1(ConstDcl_SPAN), #2(ConstDcl_SPAN))
            in
              (UserCode.GlobalDcl_PROD_2_ACT (ConstDcl_RES, ConstDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun GlobalDcl_PROD_3 (strm) = let
            val (VarDcl_RES, VarDcl_SPAN, strm') = VarDcl_NT(strm)
            val FULL_SPAN = (#1(VarDcl_SPAN), #2(VarDcl_SPAN))
            in
              (UserCode.GlobalDcl_PROD_3_ACT (VarDcl_RES, VarDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun GlobalDcl_PROD_4 (strm) = let
            val (FunctionDcl_RES, FunctionDcl_SPAN, strm') = FunctionDcl_NT(strm)
            val FULL_SPAN = (#1(FunctionDcl_SPAN), #2(FunctionDcl_SPAN))
            in
              (UserCode.GlobalDcl_PROD_4_ACT (FunctionDcl_RES, FunctionDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_function, _, strm') => GlobalDcl_PROD_4(strm)
          | (Tok.KW_const, _, strm') => GlobalDcl_PROD_2(strm)
          | (Tok.KW_input, _, strm') => GlobalDcl_PROD_1(strm)
          | (Tok.KW_bool, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_field, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_image, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_int, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_kernel, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_mat2, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_mat3, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_mat4, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_real, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_string, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_tensor, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_vec2, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_vec3, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_vec4, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_OField, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_fem, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_mesh, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_element, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.KW_fnspace, _, strm') => GlobalDcl_PROD_3(strm)
          | (Tok.ID(_), _, strm') => GlobalDcl_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
fun Program_NT (strm) = let
      fun Program_PROD_1_SUBRULE_1_NT (strm) = let
            val (GlobalDcl_RES, GlobalDcl_SPAN, strm') = GlobalDcl_NT(strm)
            val FULL_SPAN = (#1(GlobalDcl_SPAN), #2(GlobalDcl_SPAN))
            in
              ((GlobalDcl_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_bool, _, strm') => true
              | (Tok.KW_const, _, strm') => true
              | (Tok.KW_field, _, strm') => true
              | (Tok.KW_function, _, strm') => true
              | (Tok.KW_image, _, strm') => true
              | (Tok.KW_input, _, strm') => true
              | (Tok.KW_int, _, strm') => true
              | (Tok.KW_kernel, _, strm') => true
              | (Tok.KW_mat2, _, strm') => true
              | (Tok.KW_mat3, _, strm') => true
              | (Tok.KW_mat4, _, strm') => true
              | (Tok.KW_real, _, strm') => true
              | (Tok.KW_string, _, strm') => true
              | (Tok.KW_tensor, _, strm') => true
              | (Tok.KW_vec2, _, strm') => true
              | (Tok.KW_vec3, _, strm') => true
              | (Tok.KW_vec4, _, strm') => true
              | (Tok.KW_OField, _, strm') => true
              | (Tok.KW_fem, _, strm') => true
              | (Tok.KW_mesh, _, strm') => true
              | (Tok.KW_element, _, strm') => true
              | (Tok.KW_fnspace, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (GlobalDcl_RES, GlobalDcl_SPAN, strm') = EBNF.closure(Program_PROD_1_SUBRULE_1_PRED, Program_PROD_1_SUBRULE_1_NT, strm)
      fun Program_PROD_1_SUBRULE_2_NT (strm) = let
            val (InitializeBlock_RES, InitializeBlock_SPAN, strm') = InitializeBlock_NT(strm)
            val FULL_SPAN = (#1(InitializeBlock_SPAN),
              #2(InitializeBlock_SPAN))
            in
              ((InitializeBlock_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_initialize, _, strm') => true
              | _ => false
            (* end case *))
      val (InitializeBlock_RES, InitializeBlock_SPAN, strm') = EBNF.optional(Program_PROD_1_SUBRULE_2_PRED, Program_PROD_1_SUBRULE_2_NT, strm')
      val (StrandDcl_RES, StrandDcl_SPAN, strm') = StrandDcl_NT(strm')
      fun Program_PROD_1_SUBRULE_3_NT (strm) = let
            val (GlobalStart_RES, GlobalStart_SPAN, strm') = GlobalStart_NT(strm)
            val FULL_SPAN = (#1(GlobalStart_SPAN), #2(GlobalStart_SPAN))
            in
              ((GlobalStart_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_3_PRED (strm) = (case (lex(strm))
             of (Tok.KW_start, _, strm') => true
              | _ => false
            (* end case *))
      val (GlobalStart_RES, GlobalStart_SPAN, strm') = EBNF.optional(Program_PROD_1_SUBRULE_3_PRED, Program_PROD_1_SUBRULE_3_NT, strm')
      fun Program_PROD_1_SUBRULE_4_NT (strm) = let
            val (GlobalUpdate_RES, GlobalUpdate_SPAN, strm') = GlobalUpdate_NT(strm)
            val FULL_SPAN = (#1(GlobalUpdate_SPAN), #2(GlobalUpdate_SPAN))
            in
              ((GlobalUpdate_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_4_PRED (strm) = (case (lex(strm))
             of (Tok.KW_update, _, strm') => true
              | _ => false
            (* end case *))
      val (GlobalUpdate_RES, GlobalUpdate_SPAN, strm') = EBNF.optional(Program_PROD_1_SUBRULE_4_PRED, Program_PROD_1_SUBRULE_4_NT, strm')
      val (CreateStrands_RES, CreateStrands_SPAN, strm') = CreateStrands_NT(strm')
      val FULL_SPAN = (#1(GlobalDcl_SPAN), #2(CreateStrands_SPAN))
      in
        (UserCode.Program_PROD_1_ACT (GlobalStart_RES, GlobalDcl_RES, InitializeBlock_RES, StrandDcl_RES, GlobalUpdate_RES, CreateStrands_RES, GlobalStart_SPAN : (Lex.pos * Lex.pos), GlobalDcl_SPAN : (Lex.pos * Lex.pos), InitializeBlock_SPAN : (Lex.pos * Lex.pos), StrandDcl_SPAN : (Lex.pos * Lex.pos), GlobalUpdate_SPAN : (Lex.pos * Lex.pos), CreateStrands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Root_NT (strm) = let
      val (VERSION_RES, VERSION_SPAN, strm') = matchVERSION(strm)
      val (Program_RES, Program_SPAN, strm') = Program_NT(strm')
      val FULL_SPAN = (#1(VERSION_SPAN), #2(Program_SPAN))
      in
        (UserCode.Root_PROD_1_ACT (VERSION_RES, Program_RES, VERSION_SPAN : (Lex.pos * Lex.pos), Program_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (Root_NT)
end
val Root_NT =  fn s => unwrap (Err.launch (eh, lexFn, Root_NT , true) s)

in (Root_NT) end
  in
fun parse lexFn  s = let val (Root_NT) = mk lexFn in Root_NT s end

  end

end
