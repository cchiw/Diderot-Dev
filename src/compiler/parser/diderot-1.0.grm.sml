structure Diderot100Tokens =
  struct
    datatype token
      = KW_bool
      | KW_continue
      | KW_die
      | KW_else
      | KW_false
      | KW_field
      | KW_foreach
      | KW_function
      | KW_global
      | KW_identity
      | KW_if
      | KW_image
      | KW_in
      | KW_initially
      | KW_input
      | KW_int
      | KW_kernel
      | KW_load
      | KW_new
      | KW_output
      | KW_print
      | KW_real
      | KW_return
      | KW_stabilize
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
            KW_bool, KW_continue, KW_die, KW_else, KW_false, KW_field, KW_foreach, KW_function, KW_global, KW_identity, KW_if, KW_image, KW_in, KW_initially, KW_input, KW_int, KW_kernel, KW_load, KW_new, KW_output, KW_print, KW_real, KW_return, KW_stabilize, KW_strand, KW_string, KW_tensor, KW_true, KW_update, KW_vec2, KW_vec3, KW_vec4, KW_zeros, KW_OField, KW_fem, KW_mesh, KW_element, KW_fnspace, OP_eq, OP_pluseq, OP_minuseq, OP_stareq, OP_slasheq, OP_modeq, OP_orelse, OP_andalso, OP_lt, OP_lte, OP_eqeq, OP_neq, OP_gte, OP_gt, OP_plus, OP_minus, OP_star, OP_convolve, OP_dot, OP_cross, OP_outer, OP_slash, OP_mod, OP_exp, OP_comp, OP_at, OP_D, OP_Dotimes, OP_curl, OP_Ddot, LP, RP, LB, RB, LCB, RCB, COMMA, SEMI, COLON, HASH, BANG, BAR, DOT, DOTDOT, EOF
           ]
    fun toString tok =
(case (tok)
 of (KW_bool) => "bool"
  | (KW_continue) => "continue"
  | (KW_die) => "die"
  | (KW_else) => "else"
  | (KW_false) => "false"
  | (KW_field) => "field"
  | (KW_foreach) => "foreach"
  | (KW_function) => "function"
  | (KW_global) => "global"
  | (KW_identity) => "identity"
  | (KW_if) => "if"
  | (KW_image) => "image"
  | (KW_in) => "in"
  | (KW_initially) => "initially"
  | (KW_input) => "input"
  | (KW_int) => "int"
  | (KW_kernel) => "kernel"
  | (KW_load) => "load"
  | (KW_new) => "new"
  | (KW_output) => "output"
  | (KW_print) => "print"
  | (KW_real) => "real"
  | (KW_return) => "return"
  | (KW_stabilize) => "stabilize"
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
  | (KW_continue) => true
  | (KW_die) => true
  | (KW_else) => true
  | (KW_false) => false
  | (KW_field) => true
  | (KW_foreach) => true
  | (KW_function) => true
  | (KW_global) => true
  | (KW_identity) => true
  | (KW_if) => true
  | (KW_image) => true
  | (KW_in) => false
  | (KW_initially) => true
  | (KW_input) => false
  | (KW_int) => true
  | (KW_kernel) => true
  | (KW_load) => true
  | (KW_new) => true
  | (KW_output) => true
  | (KW_print) => true
  | (KW_real) => true
  | (KW_return) => true
  | (KW_stabilize) => true
  | (KW_strand) => true
  | (KW_string) => true
  | (KW_tensor) => true
  | (KW_true) => false
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
  end (* Diderot100Tokens *)

functor Diderot100ParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
Diderot100Tokens
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

  fun mkCondExp (cons : PT.expr * PT.expr -> PT.expr) = let
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
fun Program_PROD_1_ACT (StrandDecl, CoordinationDecl, GlobalDecl, GlobalUpdate, StrandDecl_SPAN : (Lex.pos * Lex.pos), CoordinationDecl_SPAN : (Lex.pos * Lex.pos), GlobalDecl_SPAN : (Lex.pos * Lex.pos), GlobalUpdate_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({
                      globals = GlobalDecl,
		      globInit = NONE,
                      strand = StrandDecl,
                      create = CoordinationDecl,
                      start = NONE,
                      update = GlobalUpdate
                    })
fun GlobalDecl_PROD_2_ACT (VarDecl, VarDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markDecl (FULL_SPAN, PT.GD_Var VarDecl))
fun InputDecl_PROD_1_ACT (SR1, SR2, SEMI, KW_input, InputType, BindId, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_input_SPAN : (Lex.pos * Lex.pos), InputType_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markDecl(FULL_SPAN, PT.GD_Input(InputType, BindId, SR1, SR2)))
fun VarDecl_PROD_1_ACT (Expr, SEMI, Type, OP_eq, BindId, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.VD_Mark (FULL_SPAN, PT.VD_Decl(Type, BindId, SOME Expr)))
fun FunDecl_PROD_1_ACT (LP, RP, Params, ConcreteType, KW_function, BindId, FunBody, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Params_SPAN : (Lex.pos * Lex.pos), ConcreteType_SPAN : (Lex.pos * Lex.pos), KW_function_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FunBody_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markDecl(FULL_SPAN, PT.GD_Func(ConcreteType, BindId, Params, FunBody)))
fun Params_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (flatten SR)
fun Param_PROD_1_ACT (ValueType, BindId, ValueType_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.P_Mark (FULL_SPAN, PT.P_Param(ValueType, BindId)))
fun FunBody_PROD_1_ACT (Expr, SEMI, OP_eq, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.FB_Expr Expr)
fun FunBody_PROD_2_ACT (Block, Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.FB_Stmt Block)
fun StrandDecl_PROD_1_ACT (LP, RP, LCB, RCB, Params, StrandMethod, StrandStateDecl, BindId, KW_strand, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Params_SPAN : (Lex.pos * Lex.pos), StrandMethod_SPAN : (Lex.pos * Lex.pos), StrandStateDecl_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), KW_strand_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.SD_Mark (FULL_SPAN, PT.SD_Strand{
                      name = BindId,
		      params = Params,
		      state = StrandStateDecl,
		      stateInit = NONE,
		      methods = StrandMethod
                    }))
fun StrandStateDecl_PROD_1_ACT (VarDecl, KW_output, VarDecl_SPAN : (Lex.pos * Lex.pos), KW_output_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.SVD_Mark (FULL_SPAN, PT.SVD_VarDcl(true, VarDecl)))
fun StrandStateDecl_PROD_2_ACT (VarDecl, VarDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.SVD_Mark (FULL_SPAN, PT.SVD_VarDcl(false, VarDecl)))
fun StrandMethod_PROD_1_ACT (Block, MethodId, Block_SPAN : (Lex.pos * Lex.pos), MethodId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.M_Mark (FULL_SPAN, PT.M_Method(MethodId, Block)))
fun MethodId_PROD_1_ACT (KW_update, KW_update_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (StrandUtil.Update)
fun MethodId_PROD_2_ACT (KW_stabilize, KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (StrandUtil.Stabilize)
fun Block_PROD_1_ACT (LCB, RCB, Stmt, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Stmt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Block Stmt))
fun Stmt_PROD_1_ACT (AtomicStmt, AtomicStmt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AtomicStmt)
fun Stmt_PROD_2_ACT (LP, RP, Stmt, Type, KW_foreach, Iterator, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Stmt_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_foreach_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Foreach(Type, Iterator, Stmt)))
fun Stmt_PROD_3_ACT (LP, RP, Expr, KW_else, KW_if, Stmt1, Stmt2, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), Stmt1_SPAN : (Lex.pos * Lex.pos), Stmt2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_IfThenElse(Expr, Stmt1, Stmt2)))
fun Stmt_PROD_4_ACT (LP, RP, Expr, Stmt, KW_if, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), Stmt_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_IfThen(Expr, Stmt)))
fun AtomicStmt_PROD_1_ACT (Block, Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Block)
fun AtomicStmt_PROD_2_ACT (VarDecl, VarDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.S_Decl VarDecl)
fun AtomicStmt_PROD_3_ACT (SEMI, KW_stabilize, SEMI_SPAN : (Lex.pos * Lex.pos), KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Stabilize))
fun AtomicStmt_PROD_4_ACT (SEMI, KW_continue, SEMI_SPAN : (Lex.pos * Lex.pos), KW_continue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Continue))
fun AtomicStmt_PROD_5_ACT (SEMI, KW_die, SEMI_SPAN : (Lex.pos * Lex.pos), KW_die_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Die))
fun AtomicStmt_PROD_6_ACT (ID, LP, RP, SEMI, Arguments, KW_new, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), KW_new_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_New(ID, Arguments)))
fun AtomicStmt_PROD_7_ACT (LP, RP, SR, Expr, SEMI, KW_print, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_print_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Print(Expr::SR)))
fun AtomicStmt_PROD_8_ACT (Expr, SEMI, OP_eq, BindId, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, PT.S_Assign(BindId, NONE, Expr)))
fun AtomicStmt_PROD_9_ACT (Expr, SEMI, OP_pluseq, BindId, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_pluseq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Assign(BindId, SOME Op.asgn_add, Expr)))
fun AtomicStmt_PROD_10_ACT (Expr, SEMI, OP_minuseq, BindId, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_minuseq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Assign(BindId, SOME Op.asgn_sub, Expr)))
fun AtomicStmt_PROD_11_ACT (Expr, SEMI, OP_stareq, BindId, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_stareq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Assign(BindId, SOME Op.asgn_mul, Expr)))
fun AtomicStmt_PROD_12_ACT (Expr, SEMI, OP_slasheq, BindId, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_slasheq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Assign(BindId, SOME Op.asgn_div, Expr)))
fun AtomicStmt_PROD_13_ACT (Expr, SEMI, OP_modeq, BindId, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_modeq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Assign(BindId, SOME Op.asgn_mod, Expr)))
fun AtomicStmt_PROD_14_ACT (Expr, SEMI, KW_return, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_return_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt(FULL_SPAN, PT.S_Return Expr))
fun Arguments_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (flatten SR)
fun CoordinationDecl_PROD_1_ACT (SR, SEMI, KW_initially, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_initially_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SR FULL_SPAN)
fun Array_PROD_1_ACT (LB, RB, BAR, Iterations, Create, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), BAR_SPAN : (Lex.pos * Lex.pos), Iterations_SPAN : (Lex.pos * Lex.pos), Create_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn span =>
                      mark PT.CR_Mark
                        (span, PT.CR_Array(
                          NONE,
                          mark PT.COMP_Mark
                            (FULL_SPAN, PT.COMP_Comprehension(Create, Iterations)))))
fun Collection_PROD_1_ACT (BAR, LCB, RCB, Iterations, Create, BAR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Iterations_SPAN : (Lex.pos * Lex.pos), Create_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn span =>
                      mark PT.CR_Mark
                        (span, PT.CR_Collection(
                          mark PT.COMP_Mark
                            (FULL_SPAN, PT.COMP_Comprehension(Create, Iterations)))))
fun Create_PROD_1_ACT (ID, LP, RP, Arguments, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Apply(PT.E_Var ID, Arguments)))
fun Iterations_PROD_1_ACT (SR, Iteration, SR_SPAN : (Lex.pos * Lex.pos), Iteration_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Iteration :: SR)
fun Iteration_PROD_1_ACT (Expr1, Expr2, KW_in, BindId, DOTDOT, Expr1_SPAN : (Lex.pos * Lex.pos), Expr2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), DOTDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.I_Mark (
                      FULL_SPAN,
                      PT.I_Iterator(BindId,
                        markExpr((#1 Expr1_SPAN, #2 Expr2_SPAN),
                        PT.E_Range(Expr1, Expr2)))))
fun GlobalUpdate_PROD_1_ACT (Block, KW_global, Block_SPAN : (Lex.pos * Lex.pos), KW_global_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markStmt (FULL_SPAN, Block))
fun Type_PROD_1_ACT (LP, RP, KW_image, Dimensions, Dimension, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Image{
                        shape = Dimensions, dim = Dimension
                      }))
fun Type_PROD_2_ACT (LP, RP, INT, HASH, KW_field, Dimensions, Dimension, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_field_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Field{
diff = SOME INT,
shape = Dimensions,
dim = Dimension
}))
fun Type_PROD_3_ACT (LP, RP, INT, HASH, Dimensions, KW_OField, Dimension, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), KW_OField_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_OField{
diff = SOME INT,
shape = Dimensions,
dim = Dimension
}))
fun Type_PROD_4_ACT (LP, RP, INT, HASH, Dimensions, Dimension, KW_fem, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), KW_fem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_FemFld{
diff = SOME INT,
shape = Dimensions,
dim = Dimension
}))
fun Type_PROD_5_ACT (INT, HASH, KW_kernel, INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_kernel_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Kernel INT))
fun Type_PROD_7_ACT (KW_mesh, KW_mesh_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Mesh))
fun Type_PROD_8_ACT (KW_element, KW_element_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_Element))
fun Type_PROD_9_ACT (KW_fnspace, KW_fnspace_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.T_FnSpace))
fun ConcreteType_PROD_1_ACT (ValueType, SeqDimensions, ValueType_SPAN : (Lex.pos * Lex.pos), SeqDimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, SeqDimensions ValueType))
fun SeqDimensions_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn ty => ty)
fun SeqDimensions_PROD_2_ACT (LCB, RCB, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn ty => PT.T_DynSeq ty)
fun SeqDimensions_PROD_3_ACT (LCB, RCB, Dimension, SeqDimensions, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), SeqDimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn ty => SeqDimensions(PT.T_Seq(ty, Dimension)))
fun InputType_PROD_1_ACT (LP, RP, KW_image, Dimensions, Dimension, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Image{
                        shape = Dimensions, dim = Dimension
                      }))
fun InputType_PROD_2_ACT (LP, RP, INT, HASH, Dimensions, Dimension, KW_fem, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), KW_fem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_FemFld{
            diff = SOME INT,
            shape = Dimensions,
            dim = Dimension
            }))
fun InputType_PROD_3_ACT (ValueType, SeqDimensions, ValueType_SPAN : (Lex.pos * Lex.pos), SeqDimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, SeqDimensions ValueType))
fun ValueType_PROD_1_ACT (Dimensions, KW_tensor, Dimensions_SPAN : (Lex.pos * Lex.pos), KW_tensor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Tensor Dimensions))
fun ValueType_PROD_2_ACT (KW_vec2, KW_vec2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 2])
fun ValueType_PROD_3_ACT (KW_vec3, KW_vec3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 3])
fun ValueType_PROD_4_ACT (KW_vec4, KW_vec4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.T_Tensor[ilit 4])
fun ValueType_PROD_5_ACT (KW_bool, KW_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Bool))
fun ValueType_PROD_6_ACT (KW_int, KW_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Int))
fun ValueType_PROD_7_ACT (KW_real, KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Real))
fun ValueType_PROD_8_ACT (KW_string, KW_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_String))
fun ValueType_PROD_9_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy(FULL_SPAN, PT.T_Id ID))
fun Dimensions_PROD_1_ACT (LB, RB, SR, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (flatten SR)
fun Dimension_PROD_1_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, ilit INT))
fun Expr_PROD_1_SUBRULE_1_PROD_1_ACT (KW_else, TestExpr, Expr1, Expr2, KW_if, KW_else_SPAN : (Lex.pos * Lex.pos), TestExpr_SPAN : (Lex.pos * Lex.pos), Expr1_SPAN : (Lex.pos * Lex.pos), Expr2_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Expr1, Expr2)
fun Expr_PROD_1_ACT (SR, TestExpr, SR_SPAN : (Lex.pos * Lex.pos), TestExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of NONE => TestExpr
                      | SOME(e1, e2) => markExpr(FULL_SPAN, PT.E_Cond(TestExpr, e1, e2))
                    )
fun TestExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_orelse, AndExpr, OP_orelse_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (#1 FULL_SPAN, AndExpr)
fun TestExpr_PROD_1_ACT (SR, AndExpr, SR_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkCondExp PT.E_OrElse (#1 AndExpr_SPAN, AndExpr, SR, #2 SR_SPAN))
fun AndExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_andalso, CmpExpr, OP_andalso_SPAN : (Lex.pos * Lex.pos), CmpExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (#1 FULL_SPAN, CmpExpr)
fun AndExpr_PROD_1_ACT (SR, CmpExpr, SR_SPAN : (Lex.pos * Lex.pos), CmpExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkCondExp PT.E_AndAlso (#1 CmpExpr_SPAN, CmpExpr, SR, #2 SR_SPAN))
fun CmpExpr_PROD_1_SUBRULE_1_PROD_1_ACT (CmpOp, AddExpr, CmpOp_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (CmpOp, AddExpr, #2 AddExpr_SPAN)
fun CmpExpr_PROD_1_ACT (SR, AddExpr, SR_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 AddExpr_SPAN, AddExpr, SR))
fun CmpOp_PROD_1_ACT (OP_lt, OP_lt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_lt)
fun CmpOp_PROD_2_ACT (OP_lte, OP_lte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_lte)
fun CmpOp_PROD_3_ACT (OP_eqeq, OP_eqeq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_equ)
fun CmpOp_PROD_4_ACT (OP_neq, OP_neq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_neq)
fun CmpOp_PROD_5_ACT (OP_gte, OP_gte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_gte)
fun CmpOp_PROD_6_ACT (OP_gt, OP_gt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_gt)
fun AddExpr_PROD_1_SUBRULE_1_PROD_1_ACT (MulExpr, AddOp, MulExpr_SPAN : (Lex.pos * Lex.pos), AddOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AddOp, MulExpr, #2 MulExpr_SPAN)
fun AddExpr_PROD_1_ACT (SR, MulExpr, SR_SPAN : (Lex.pos * Lex.pos), MulExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 MulExpr_SPAN, MulExpr, SR))
fun AddOp_PROD_1_ACT (OP_plus, OP_plus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_add)
fun AddOp_PROD_2_ACT (OP_minus, OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_sub)
fun AddOp_PROD_3_ACT (OP_at, OP_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_at)
fun MulExpr_PROD_1_SUBRULE_1_PROD_1_ACT (MulOp, PrefixExpr, MulOp_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (MulOp, PrefixExpr, #2 PrefixExpr_SPAN)
fun MulExpr_PROD_1_ACT (SR, PrefixExpr, SR_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 PrefixExpr_SPAN, PrefixExpr, SR))
fun MulOp_PROD_1_ACT (OP_star, OP_star_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_mul)
fun MulOp_PROD_2_ACT (OP_slash, OP_slash_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_div)
fun MulOp_PROD_3_ACT (OP_mod, OP_mod_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_mod)
fun MulOp_PROD_4_ACT (OP_convolve, OP_convolve_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_convolve)
fun MulOp_PROD_5_ACT (OP_dot, OP_dot_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_dot)
fun MulOp_PROD_6_ACT (OP_cross, OP_cross_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_cross)
fun MulOp_PROD_7_ACT (OP_outer, OP_outer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_outer)
fun MulOp_PROD_8_ACT (COLON, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_colon)
fun MulOp_PROD_9_ACT (OP_comp, OP_comp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_comp)
fun PrefixExpr_PROD_1_ACT (PowerExpr, PowerExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PowerExpr)
fun PrefixExpr_PROD_2_ACT (OP_minus, PrefixExpr, OP_minus_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_UnaryOp(Op.op_neg, PrefixExpr)))
fun PrefixExpr_PROD_3_ACT (BANG, PrefixExpr, BANG_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_UnaryOp(Op.op_not, PrefixExpr)))
fun PowerExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_exp, SuffixExpr, OP_exp_SPAN : (Lex.pos * Lex.pos), SuffixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.op_pow, #1 SuffixExpr_SPAN, SuffixExpr)
fun PowerExpr_PROD_1_ACT (SR, SuffixExpr, SR_SPAN : (Lex.pos * Lex.pos), SuffixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkRBinExp (#1 FULL_SPAN, SuffixExpr, SR, #2 FULL_SPAN))
fun SuffixExpr_PROD_1_ACT (Suffix, DerivExpr, Suffix_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case Suffix
                     of [] => DerivExpr
                      | ss => markExpr(FULL_SPAN, List.foldl (fn (f, e) => f e) DerivExpr ss)
                    )
fun Suffix_PROD_1_ACT (LP, RP, Arguments, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => markExpr(FULL_SPAN, PT.E_Apply(e, Arguments)))
fun Suffix_PROD_2_ACT (LB, RB, Indices, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Indices_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => PT.E_Subscript(e, Indices))
fun Suffix_PROD_3_ACT (ID, DOT, ID_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn e => PT.E_Select(e, ID))
fun Suffix_PROD_4_ACT (SR, LCB, RCB, Expr, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
		     of NONE => (fn e => PT.E_Subscript(e, [SOME Expr]))
		      | SOME iter =>
			(fn e => PT.E_Apply(e, [PT.E_SeqComp(PT.COMP_Comprehension(Expr, [iter]))]))
		    )
fun Iterator_PROD_1_ACT (Expr, KW_in, BindId, Expr_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.I_Mark (FULL_SPAN, PT.I_Iterator(BindId, Expr)))
fun Indices_PROD_1_ACT (SR, Index, SR_SPAN : (Lex.pos * Lex.pos), Index_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Index :: SR)
fun Index_PROD_1_ACT (COLON, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NONE)
fun Index_PROD_2_ACT (Expr, Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SOME Expr)
fun DerivExpr_PROD_1_ACT (AtomExpr, AtomExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AtomExpr)
fun DerivExpr_PROD_2_ACT (OP_D, DerivExpr, OP_D_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_UnaryOp(Op.op_D, DerivExpr)))
fun DerivExpr_PROD_3_ACT (OP_Dotimes, DerivExpr, OP_Dotimes_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_UnaryOp(Op.op_Dotimes, DerivExpr)))
fun DerivExpr_PROD_4_ACT (OP_curl, DerivExpr, OP_curl_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_UnaryOp(Op.op_curl, DerivExpr)))
fun DerivExpr_PROD_5_ACT (OP_Ddot, DerivExpr, OP_Ddot_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_UnaryOp(Op.op_Ddot, DerivExpr)))
fun AtomExpr_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Var ID))
fun AtomExpr_PROD_2_ACT (LB, RB, KW_identity, Dimension, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), KW_identity_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Id Dimension))
fun AtomExpr_PROD_3_ACT (KW_zeros, Dimensions, KW_zeros_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Zero Dimensions))
fun AtomExpr_PROD_4_ACT (LP, RP, Expr, KW_real, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Real Expr))
fun AtomExpr_PROD_5_ACT (LoadExpr, LoadExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (LoadExpr)
fun AtomExpr_PROD_6_ACT (LP, RP, Expr, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Expr)
fun AtomExpr_PROD_7_ACT (LCB, RCB, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Sequence[]))
fun AtomExpr_PROD_8_ACT (SR, LCB, RCB, Expr, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Sequence(Expr::SR)))
fun AtomExpr_PROD_9_ACT (LB, RB, SR, Expr, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Cons(Expr::SR)))
fun AtomExpr_PROD_10_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Int INT)))
fun AtomExpr_PROD_11_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Real REAL)))
fun AtomExpr_PROD_12_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.String STRING)))
fun AtomExpr_PROD_13_ACT (KW_true, KW_true_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Bool true)))
fun AtomExpr_PROD_14_ACT (KW_false, KW_false_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Bool false)))
fun AtomExpr_PROD_15_ACT (BAR1, BAR2, Expr, BAR1_SPAN : (Lex.pos * Lex.pos), BAR2_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_UnaryOp(Op.op_norm, Expr)))
fun Initializer_PROD_1_ACT (LB, RB, KW_identity, Dimension, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), KW_identity_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Id Dimension))
fun Initializer_PROD_2_ACT (KW_zeros, Dimensions, KW_zeros_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Zero Dimensions))
fun Initializer_PROD_3_ACT (LoadExpr, LoadExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (LoadExpr)
fun Initializer_PROD_4_ACT (SR, LCB, RCB, Initializer, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Initializer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Sequence(Initializer::SR)))
fun Initializer_PROD_5_ACT (LB, RB, SR, Initializer, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Initializer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Cons(Initializer::SR)))
fun Initializer_PROD_6_ACT (INT, OP_minus, INT_SPAN : (Lex.pos * Lex.pos), OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Int(~INT))))
fun Initializer_PROD_7_ACT (REAL, OP_minus, REAL_SPAN : (Lex.pos * Lex.pos), OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Real(RealLit.negate REAL))))
fun Initializer_PROD_8_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Int INT)))
fun Initializer_PROD_9_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Real REAL)))
fun Initializer_PROD_10_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.String STRING)))
fun Initializer_PROD_11_ACT (KW_true, KW_true_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Bool true)))
fun Initializer_PROD_12_ACT (KW_false, KW_false_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_Lit(L.Bool false)))
fun LoadExpr_PROD_1_ACT (LP, RP, Path, KW_image, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Path_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_LoadImage Path))
fun LoadExpr_PROD_2_ACT (LP, RP, Path, KW_load, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Path_SPAN : (Lex.pos * Lex.pos), KW_load_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr(FULL_SPAN, PT.E_LoadSeq Path))
fun Path_PROD_1_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExpr (FULL_SPAN, PT.E_Lit(L.String STRING)))
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
fun matchKW_continue strm = (case (lex(strm))
 of (Tok.KW_continue, span, strm') => ((), span, strm')
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
fun matchKW_global strm = (case (lex(strm))
 of (Tok.KW_global, span, strm') => ((), span, strm')
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
fun matchKW_initially strm = (case (lex(strm))
 of (Tok.KW_initially, span, strm') => ((), span, strm')
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
fun matchKW_load strm = (case (lex(strm))
 of (Tok.KW_load, span, strm') => ((), span, strm')
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
fun BindId_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
      in
        (UserCode.BindId_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Path_NT (strm) = let
      val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
      val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
      in
        (UserCode.Path_PROD_1_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun LoadExpr_NT (strm) = let
      fun LoadExpr_PROD_1 (strm) = let
            val (KW_image_RES, KW_image_SPAN, strm') = matchKW_image(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Path_RES, Path_SPAN, strm') = Path_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_image_SPAN), #2(RP_SPAN))
            in
              (UserCode.LoadExpr_PROD_1_ACT (LP_RES, RP_RES, Path_RES, KW_image_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Path_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun LoadExpr_PROD_2 (strm) = let
            val (KW_load_RES, KW_load_SPAN, strm') = matchKW_load(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Path_RES, Path_SPAN, strm') = Path_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_load_SPAN), #2(RP_SPAN))
            in
              (UserCode.LoadExpr_PROD_2_ACT (LP_RES, RP_RES, Path_RES, KW_load_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Path_SPAN : (Lex.pos * Lex.pos), KW_load_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_load, _, strm') => LoadExpr_PROD_2(strm)
          | (Tok.KW_image, _, strm') => LoadExpr_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Dimension_NT (strm) = let
      val (INT_RES, INT_SPAN, strm') = matchINT(strm)
      val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
      in
        (UserCode.Dimension_PROD_1_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Dimensions_NT (strm) = let
      val (LB_RES, LB_SPAN, strm') = matchLB(strm)
      fun Dimensions_PROD_1_SUBRULE_1_NT (strm) = let
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm)
            fun Dimensions_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Dimension_SPAN))
                  in
                    ((Dimension_RES), FULL_SPAN, strm')
                  end
            fun Dimensions_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Dimensions_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, Dimensions_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Dimension_SPAN), #2(SR_SPAN))
            in
              ((Dimension_RES, SR_RES), FULL_SPAN, strm')
            end
      fun Dimensions_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.INT(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Dimensions_PROD_1_SUBRULE_1_PRED, Dimensions_PROD_1_SUBRULE_1_NT, strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
      in
        (UserCode.Dimensions_PROD_1_ACT (LB_RES, RB_RES, SR_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun MulOp_NT (strm) = let
      fun MulOp_PROD_1 (strm) = let
            val (OP_star_RES, OP_star_SPAN, strm') = matchOP_star(strm)
            val FULL_SPAN = (#1(OP_star_SPAN), #2(OP_star_SPAN))
            in
              (UserCode.MulOp_PROD_1_ACT (OP_star_RES, OP_star_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_2 (strm) = let
            val (OP_slash_RES, OP_slash_SPAN, strm') = matchOP_slash(strm)
            val FULL_SPAN = (#1(OP_slash_SPAN), #2(OP_slash_SPAN))
            in
              (UserCode.MulOp_PROD_2_ACT (OP_slash_RES, OP_slash_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_3 (strm) = let
            val (OP_mod_RES, OP_mod_SPAN, strm') = matchOP_mod(strm)
            val FULL_SPAN = (#1(OP_mod_SPAN), #2(OP_mod_SPAN))
            in
              (UserCode.MulOp_PROD_3_ACT (OP_mod_RES, OP_mod_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_4 (strm) = let
            val (OP_convolve_RES, OP_convolve_SPAN, strm') = matchOP_convolve(strm)
            val FULL_SPAN = (#1(OP_convolve_SPAN), #2(OP_convolve_SPAN))
            in
              (UserCode.MulOp_PROD_4_ACT (OP_convolve_RES, OP_convolve_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_5 (strm) = let
            val (OP_dot_RES, OP_dot_SPAN, strm') = matchOP_dot(strm)
            val FULL_SPAN = (#1(OP_dot_SPAN), #2(OP_dot_SPAN))
            in
              (UserCode.MulOp_PROD_5_ACT (OP_dot_RES, OP_dot_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_6 (strm) = let
            val (OP_cross_RES, OP_cross_SPAN, strm') = matchOP_cross(strm)
            val FULL_SPAN = (#1(OP_cross_SPAN), #2(OP_cross_SPAN))
            in
              (UserCode.MulOp_PROD_6_ACT (OP_cross_RES, OP_cross_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_7 (strm) = let
            val (OP_outer_RES, OP_outer_SPAN, strm') = matchOP_outer(strm)
            val FULL_SPAN = (#1(OP_outer_SPAN), #2(OP_outer_SPAN))
            in
              (UserCode.MulOp_PROD_7_ACT (OP_outer_RES, OP_outer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_8 (strm) = let
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm)
            val FULL_SPAN = (#1(COLON_SPAN), #2(COLON_SPAN))
            in
              (UserCode.MulOp_PROD_8_ACT (COLON_RES, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_9 (strm) = let
            val (OP_comp_RES, OP_comp_SPAN, strm') = matchOP_comp(strm)
            val FULL_SPAN = (#1(OP_comp_SPAN), #2(OP_comp_SPAN))
            in
              (UserCode.MulOp_PROD_9_ACT (OP_comp_RES, OP_comp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_comp, _, strm') => MulOp_PROD_9(strm)
          | (Tok.OP_outer, _, strm') => MulOp_PROD_7(strm)
          | (Tok.OP_dot, _, strm') => MulOp_PROD_5(strm)
          | (Tok.OP_mod, _, strm') => MulOp_PROD_3(strm)
          | (Tok.OP_star, _, strm') => MulOp_PROD_1(strm)
          | (Tok.OP_slash, _, strm') => MulOp_PROD_2(strm)
          | (Tok.OP_convolve, _, strm') => MulOp_PROD_4(strm)
          | (Tok.OP_cross, _, strm') => MulOp_PROD_6(strm)
          | (Tok.COLON, _, strm') => MulOp_PROD_8(strm)
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
fun CmpOp_NT (strm) = let
      fun CmpOp_PROD_1 (strm) = let
            val (OP_lt_RES, OP_lt_SPAN, strm') = matchOP_lt(strm)
            val FULL_SPAN = (#1(OP_lt_SPAN), #2(OP_lt_SPAN))
            in
              (UserCode.CmpOp_PROD_1_ACT (OP_lt_RES, OP_lt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CmpOp_PROD_2 (strm) = let
            val (OP_lte_RES, OP_lte_SPAN, strm') = matchOP_lte(strm)
            val FULL_SPAN = (#1(OP_lte_SPAN), #2(OP_lte_SPAN))
            in
              (UserCode.CmpOp_PROD_2_ACT (OP_lte_RES, OP_lte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CmpOp_PROD_3 (strm) = let
            val (OP_eqeq_RES, OP_eqeq_SPAN, strm') = matchOP_eqeq(strm)
            val FULL_SPAN = (#1(OP_eqeq_SPAN), #2(OP_eqeq_SPAN))
            in
              (UserCode.CmpOp_PROD_3_ACT (OP_eqeq_RES, OP_eqeq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CmpOp_PROD_4 (strm) = let
            val (OP_neq_RES, OP_neq_SPAN, strm') = matchOP_neq(strm)
            val FULL_SPAN = (#1(OP_neq_SPAN), #2(OP_neq_SPAN))
            in
              (UserCode.CmpOp_PROD_4_ACT (OP_neq_RES, OP_neq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CmpOp_PROD_5 (strm) = let
            val (OP_gte_RES, OP_gte_SPAN, strm') = matchOP_gte(strm)
            val FULL_SPAN = (#1(OP_gte_SPAN), #2(OP_gte_SPAN))
            in
              (UserCode.CmpOp_PROD_5_ACT (OP_gte_RES, OP_gte_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CmpOp_PROD_6 (strm) = let
            val (OP_gt_RES, OP_gt_SPAN, strm') = matchOP_gt(strm)
            val FULL_SPAN = (#1(OP_gt_SPAN), #2(OP_gt_SPAN))
            in
              (UserCode.CmpOp_PROD_6_ACT (OP_gt_RES, OP_gt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_gt, _, strm') => CmpOp_PROD_6(strm)
          | (Tok.OP_neq, _, strm') => CmpOp_PROD_4(strm)
          | (Tok.OP_lte, _, strm') => CmpOp_PROD_2(strm)
          | (Tok.OP_lt, _, strm') => CmpOp_PROD_1(strm)
          | (Tok.OP_eqeq, _, strm') => CmpOp_PROD_3(strm)
          | (Tok.OP_gte, _, strm') => CmpOp_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
fun Expr_NT (strm) = let
      val (TestExpr_RES, TestExpr_SPAN, strm') = TestExpr_NT(strm)
      fun Expr_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (Expr1_RES, Expr1_SPAN, strm') = Expr_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (Expr2_RES, Expr2_SPAN, strm') = Expr_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(Expr2_SPAN))
            in
              (UserCode.Expr_PROD_1_SUBRULE_1_PROD_1_ACT (KW_else_RES, TestExpr_RES, Expr1_RES, Expr2_RES, KW_if_RES, KW_else_SPAN : (Lex.pos * Lex.pos), TestExpr_SPAN : (Lex.pos * Lex.pos), Expr1_SPAN : (Lex.pos * Lex.pos), Expr2_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Expr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_if, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Expr_PROD_1_SUBRULE_1_PRED, Expr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(TestExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.Expr_PROD_1_ACT (SR_RES, TestExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), TestExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and TestExpr_NT (strm) = let
      val (AndExpr_RES, AndExpr_SPAN, strm') = AndExpr_NT(strm)
      fun TestExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (OP_orelse_RES, OP_orelse_SPAN, strm') = matchOP_orelse(strm)
            val (AndExpr_RES, AndExpr_SPAN, strm') = AndExpr_NT(strm')
            val FULL_SPAN = (#1(OP_orelse_SPAN), #2(AndExpr_SPAN))
            in
              (UserCode.TestExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_orelse_RES, AndExpr_RES, OP_orelse_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TestExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_orelse, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TestExpr_PROD_1_SUBRULE_1_PRED, TestExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AndExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.TestExpr_PROD_1_ACT (SR_RES, AndExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), AndExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AndExpr_NT (strm) = let
      val (CmpExpr_RES, CmpExpr_SPAN, strm') = CmpExpr_NT(strm)
      fun AndExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (OP_andalso_RES, OP_andalso_SPAN, strm') = matchOP_andalso(strm)
            val (CmpExpr_RES, CmpExpr_SPAN, strm') = CmpExpr_NT(strm')
            val FULL_SPAN = (#1(OP_andalso_SPAN), #2(CmpExpr_SPAN))
            in
              (UserCode.AndExpr_PROD_1_SUBRULE_1_PROD_1_ACT (OP_andalso_RES, CmpExpr_RES, OP_andalso_SPAN : (Lex.pos * Lex.pos), CmpExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AndExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_andalso, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AndExpr_PROD_1_SUBRULE_1_PRED, AndExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(CmpExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.AndExpr_PROD_1_ACT (SR_RES, CmpExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), CmpExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and CmpExpr_NT (strm) = let
      val (AddExpr_RES, AddExpr_SPAN, strm') = AddExpr_NT(strm)
      fun CmpExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (CmpOp_RES, CmpOp_SPAN, strm') = CmpOp_NT(strm)
            val (AddExpr_RES, AddExpr_SPAN, strm') = AddExpr_NT(strm')
            val FULL_SPAN = (#1(CmpOp_SPAN), #2(AddExpr_SPAN))
            in
              (UserCode.CmpExpr_PROD_1_SUBRULE_1_PROD_1_ACT (CmpOp_RES, AddExpr_RES, CmpOp_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun CmpExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_lt, _, strm') => true
              | (Tok.OP_lte, _, strm') => true
              | (Tok.OP_eqeq, _, strm') => true
              | (Tok.OP_neq, _, strm') => true
              | (Tok.OP_gte, _, strm') => true
              | (Tok.OP_gt, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(CmpExpr_PROD_1_SUBRULE_1_PRED, CmpExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AddExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.CmpExpr_PROD_1_ACT (SR_RES, AddExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), AddExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AddExpr_NT (strm) = let
      val (MulExpr_RES, MulExpr_SPAN, strm') = MulExpr_NT(strm)
      fun AddExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (AddOp_RES, AddOp_SPAN, strm') = AddOp_NT(strm)
            val (MulExpr_RES, MulExpr_SPAN, strm') = MulExpr_NT(strm')
            val FULL_SPAN = (#1(AddOp_SPAN), #2(MulExpr_SPAN))
            in
              (UserCode.AddExpr_PROD_1_SUBRULE_1_PROD_1_ACT (MulExpr_RES, AddOp_RES, MulExpr_SPAN : (Lex.pos * Lex.pos), AddOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AddExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OP_plus, _, strm') => true
              | (Tok.OP_minus, _, strm') => true
              | (Tok.OP_at, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AddExpr_PROD_1_SUBRULE_1_PRED, AddExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(MulExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.AddExpr_PROD_1_ACT (SR_RES, MulExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), MulExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and MulExpr_NT (strm) = let
      val (PrefixExpr_RES, PrefixExpr_SPAN, strm') = PrefixExpr_NT(strm)
      fun MulExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (MulOp_RES, MulOp_SPAN, strm') = MulOp_NT(strm)
            val (PrefixExpr_RES, PrefixExpr_SPAN, strm') = PrefixExpr_NT(strm')
            val FULL_SPAN = (#1(MulOp_SPAN), #2(PrefixExpr_SPAN))
            in
              (UserCode.MulExpr_PROD_1_SUBRULE_1_PROD_1_ACT (MulOp_RES, PrefixExpr_RES, MulOp_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
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
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(MulExpr_PROD_1_SUBRULE_1_PRED, MulExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(PrefixExpr_SPAN), #2(SR_SPAN))
      in
        (UserCode.MulExpr_PROD_1_ACT (SR_RES, PrefixExpr_RES, SR_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
            val (OP_minus_RES, OP_minus_SPAN, strm') = matchOP_minus(strm)
            val (PrefixExpr_RES, PrefixExpr_SPAN, strm') = PrefixExpr_NT(strm')
            val FULL_SPAN = (#1(OP_minus_SPAN), #2(PrefixExpr_SPAN))
            in
              (UserCode.PrefixExpr_PROD_2_ACT (OP_minus_RES, PrefixExpr_RES, OP_minus_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrefixExpr_PROD_3 (strm) = let
            val (BANG_RES, BANG_SPAN, strm') = matchBANG(strm)
            val (PrefixExpr_RES, PrefixExpr_SPAN, strm') = PrefixExpr_NT(strm')
            val FULL_SPAN = (#1(BANG_SPAN), #2(PrefixExpr_SPAN))
            in
              (UserCode.PrefixExpr_PROD_3_ACT (BANG_RES, PrefixExpr_RES, BANG_SPAN : (Lex.pos * Lex.pos), PrefixExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.BANG, _, strm') => PrefixExpr_PROD_3(strm)
          | (Tok.KW_false, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_identity, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_image, _, strm') => PrefixExpr_PROD_1(strm)
          | (Tok.KW_load, _, strm') => PrefixExpr_PROD_1(strm)
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
          | (Tok.OP_minus, _, strm') => PrefixExpr_PROD_2(strm)
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
      val (DerivExpr_RES, DerivExpr_SPAN, strm') = DerivExpr_NT(strm)
      fun SuffixExpr_PROD_1_SUBRULE_1_NT (strm) = let
            val (Suffix_RES, Suffix_SPAN, strm') = Suffix_NT(strm)
            val FULL_SPAN = (#1(Suffix_SPAN), #2(Suffix_SPAN))
            in
              ((Suffix_RES), FULL_SPAN, strm')
            end
      fun SuffixExpr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LP, _, strm') => true
              | (Tok.LB, _, strm') => true
              | (Tok.LCB, _, strm') => true
              | (Tok.DOT, _, strm') => true
              | _ => false
            (* end case *))
      val (Suffix_RES, Suffix_SPAN, strm') = EBNF.closure(SuffixExpr_PROD_1_SUBRULE_1_PRED, SuffixExpr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(DerivExpr_SPAN), #2(Suffix_SPAN))
      in
        (UserCode.SuffixExpr_PROD_1_ACT (Suffix_RES, DerivExpr_RES, Suffix_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
      fun Suffix_PROD_4 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            fun Suffix_PROD_4_SUBRULE_1_NT (strm) = let
                  val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
                  val (Iterator_RES, Iterator_SPAN, strm') = Iterator_NT(strm')
                  val FULL_SPAN = (#1(BAR_SPAN), #2(Iterator_SPAN))
                  in
                    ((Iterator_RES), FULL_SPAN, strm')
                  end
            fun Suffix_PROD_4_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.BAR, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(Suffix_PROD_4_SUBRULE_1_PRED, Suffix_PROD_4_SUBRULE_1_NT, strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.Suffix_PROD_4_ACT (SR_RES, LCB_RES, RCB_RES, Expr_RES, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LCB, _, strm') => Suffix_PROD_4(strm)
          | (Tok.LB, _, strm') => Suffix_PROD_2(strm)
          | (Tok.LP, _, strm') => Suffix_PROD_1(strm)
          | (Tok.DOT, _, strm') => Suffix_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
and Iterator_NT (strm) = let
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
      val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
      val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
      val FULL_SPAN = (#1(BindId_SPAN), #2(Expr_SPAN))
      in
        (UserCode.Iterator_PROD_1_ACT (Expr_RES, KW_in_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Indices_NT (strm) = let
      val (Index_RES, Index_SPAN, strm') = Index_NT(strm)
      fun Indices_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Index_RES, Index_SPAN, strm') = Index_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Index_SPAN))
            in
              ((Index_RES), FULL_SPAN, strm')
            end
      fun Indices_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Indices_PROD_1_SUBRULE_1_PRED, Indices_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(Index_SPAN), #2(SR_SPAN))
      in
        (UserCode.Indices_PROD_1_ACT (SR_RES, Index_RES, SR_SPAN : (Lex.pos * Lex.pos), Index_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Index_NT (strm) = let
      fun Index_PROD_1 (strm) = let
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm)
            val FULL_SPAN = (#1(COLON_SPAN), #2(COLON_SPAN))
            in
              (UserCode.Index_PROD_1_ACT (COLON_RES, COLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Index_PROD_2 (strm) = let
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm)
            val FULL_SPAN = (#1(Expr_SPAN), #2(Expr_SPAN))
            in
              (UserCode.Index_PROD_2_ACT (Expr_RES, Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_false, _, strm') => Index_PROD_2(strm)
          | (Tok.KW_identity, _, strm') => Index_PROD_2(strm)
          | (Tok.KW_image, _, strm') => Index_PROD_2(strm)
          | (Tok.KW_load, _, strm') => Index_PROD_2(strm)
          | (Tok.KW_real, _, strm') => Index_PROD_2(strm)
          | (Tok.KW_true, _, strm') => Index_PROD_2(strm)
          | (Tok.KW_zeros, _, strm') => Index_PROD_2(strm)
          | (Tok.OP_minus, _, strm') => Index_PROD_2(strm)
          | (Tok.OP_D, _, strm') => Index_PROD_2(strm)
          | (Tok.OP_Dotimes, _, strm') => Index_PROD_2(strm)
          | (Tok.OP_curl, _, strm') => Index_PROD_2(strm)
          | (Tok.OP_Ddot, _, strm') => Index_PROD_2(strm)
          | (Tok.LP, _, strm') => Index_PROD_2(strm)
          | (Tok.LB, _, strm') => Index_PROD_2(strm)
          | (Tok.LCB, _, strm') => Index_PROD_2(strm)
          | (Tok.BANG, _, strm') => Index_PROD_2(strm)
          | (Tok.BAR, _, strm') => Index_PROD_2(strm)
          | (Tok.ID(_), _, strm') => Index_PROD_2(strm)
          | (Tok.INT(_), _, strm') => Index_PROD_2(strm)
          | (Tok.REAL(_), _, strm') => Index_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => Index_PROD_2(strm)
          | (Tok.COLON, _, strm') => Index_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and Arguments_NT (strm) = let
      fun Arguments_PROD_1_SUBRULE_1_NT (strm) = let
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm)
            fun Arguments_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Expr_SPAN))
                  in
                    ((Expr_RES), FULL_SPAN, strm')
                  end
            fun Arguments_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Arguments_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, Arguments_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Expr_SPAN), #2(SR_SPAN))
            in
              ((Expr_RES, SR_RES), FULL_SPAN, strm')
            end
      fun Arguments_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_false, _, strm') => true
              | (Tok.KW_identity, _, strm') => true
              | (Tok.KW_image, _, strm') => true
              | (Tok.KW_load, _, strm') => true
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
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Arguments_PROD_1_SUBRULE_1_PRED, Arguments_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.Arguments_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and DerivExpr_NT (strm) = let
      fun DerivExpr_PROD_1 (strm) = let
            val (AtomExpr_RES, AtomExpr_SPAN, strm') = AtomExpr_NT(strm)
            val FULL_SPAN = (#1(AtomExpr_SPAN), #2(AtomExpr_SPAN))
            in
              (UserCode.DerivExpr_PROD_1_ACT (AtomExpr_RES, AtomExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DerivExpr_PROD_2 (strm) = let
            val (OP_D_RES, OP_D_SPAN, strm') = matchOP_D(strm)
            val (DerivExpr_RES, DerivExpr_SPAN, strm') = DerivExpr_NT(strm')
            val FULL_SPAN = (#1(OP_D_SPAN), #2(DerivExpr_SPAN))
            in
              (UserCode.DerivExpr_PROD_2_ACT (OP_D_RES, DerivExpr_RES, OP_D_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DerivExpr_PROD_3 (strm) = let
            val (OP_Dotimes_RES, OP_Dotimes_SPAN, strm') = matchOP_Dotimes(strm)
            val (DerivExpr_RES, DerivExpr_SPAN, strm') = DerivExpr_NT(strm')
            val FULL_SPAN = (#1(OP_Dotimes_SPAN), #2(DerivExpr_SPAN))
            in
              (UserCode.DerivExpr_PROD_3_ACT (OP_Dotimes_RES, DerivExpr_RES, OP_Dotimes_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DerivExpr_PROD_4 (strm) = let
            val (OP_curl_RES, OP_curl_SPAN, strm') = matchOP_curl(strm)
            val (DerivExpr_RES, DerivExpr_SPAN, strm') = DerivExpr_NT(strm')
            val FULL_SPAN = (#1(OP_curl_SPAN), #2(DerivExpr_SPAN))
            in
              (UserCode.DerivExpr_PROD_4_ACT (OP_curl_RES, DerivExpr_RES, OP_curl_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun DerivExpr_PROD_5 (strm) = let
            val (OP_Ddot_RES, OP_Ddot_SPAN, strm') = matchOP_Ddot(strm)
            val (DerivExpr_RES, DerivExpr_SPAN, strm') = DerivExpr_NT(strm')
            val FULL_SPAN = (#1(OP_Ddot_SPAN), #2(DerivExpr_SPAN))
            in
              (UserCode.DerivExpr_PROD_5_ACT (OP_Ddot_RES, DerivExpr_RES, OP_Ddot_SPAN : (Lex.pos * Lex.pos), DerivExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.OP_Ddot, _, strm') => DerivExpr_PROD_5(strm)
          | (Tok.OP_Dotimes, _, strm') => DerivExpr_PROD_3(strm)
          | (Tok.KW_false, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.KW_identity, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.KW_image, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.KW_load, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.KW_real, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.KW_true, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.KW_zeros, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.LP, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.LB, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.LCB, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.BAR, _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.ID(_), _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.INT(_), _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => DerivExpr_PROD_1(strm)
          | (Tok.OP_D, _, strm') => DerivExpr_PROD_2(strm)
          | (Tok.OP_curl, _, strm') => DerivExpr_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
and AtomExpr_NT (strm) = let
      fun AtomExpr_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.AtomExpr_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_2 (strm) = let
            val (KW_identity_RES, KW_identity_SPAN, strm') = matchKW_identity(strm)
            val (LB_RES, LB_SPAN, strm') = matchLB(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(KW_identity_SPAN), #2(RB_SPAN))
            in
              (UserCode.AtomExpr_PROD_2_ACT (LB_RES, RB_RES, KW_identity_RES, Dimension_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), KW_identity_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_3 (strm) = let
            val (KW_zeros_RES, KW_zeros_SPAN, strm') = matchKW_zeros(strm)
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_zeros_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.AtomExpr_PROD_3_ACT (KW_zeros_RES, Dimensions_RES, KW_zeros_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_4 (strm) = let
            val (KW_real_RES, KW_real_SPAN, strm') = matchKW_real(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_real_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomExpr_PROD_4_ACT (LP_RES, RP_RES, Expr_RES, KW_real_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_5 (strm) = let
            val (LoadExpr_RES, LoadExpr_SPAN, strm') = LoadExpr_NT(strm)
            val FULL_SPAN = (#1(LoadExpr_SPAN), #2(LoadExpr_SPAN))
            in
              (UserCode.AtomExpr_PROD_5_ACT (LoadExpr_RES, LoadExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_6 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomExpr_PROD_6_ACT (LP_RES, RP_RES, Expr_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_7 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.AtomExpr_PROD_7_ACT (LCB_RES, RCB_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_8 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            fun AtomExpr_PROD_8_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Expr_SPAN))
                  in
                    ((Expr_RES), FULL_SPAN, strm')
                  end
            fun AtomExpr_PROD_8_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomExpr_PROD_8_SUBRULE_1_PRED, AtomExpr_PROD_8_SUBRULE_1_NT, strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.AtomExpr_PROD_8_ACT (SR_RES, LCB_RES, RCB_RES, Expr_RES, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_9 (strm) = let
            val (LB_RES, LB_SPAN, strm') = matchLB(strm)
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            fun AtomExpr_PROD_9_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Expr_SPAN))
                  in
                    ((Expr_RES), FULL_SPAN, strm')
                  end
            fun AtomExpr_PROD_9_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomExpr_PROD_9_SUBRULE_1_PRED, AtomExpr_PROD_9_SUBRULE_1_NT, strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
            in
              (UserCode.AtomExpr_PROD_9_ACT (LB_RES, RB_RES, SR_RES, Expr_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_10 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.AtomExpr_PROD_10_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_11 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.AtomExpr_PROD_11_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_12 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.AtomExpr_PROD_12_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_13 (strm) = let
            val (KW_true_RES, KW_true_SPAN, strm') = matchKW_true(strm)
            val FULL_SPAN = (#1(KW_true_SPAN), #2(KW_true_SPAN))
            in
              (UserCode.AtomExpr_PROD_13_ACT (KW_true_RES, KW_true_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_14 (strm) = let
            val (KW_false_RES, KW_false_SPAN, strm') = matchKW_false(strm)
            val FULL_SPAN = (#1(KW_false_SPAN), #2(KW_false_SPAN))
            in
              (UserCode.AtomExpr_PROD_14_ACT (KW_false_RES, KW_false_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomExpr_PROD_15 (strm) = let
            val (BAR1_RES, BAR1_SPAN, strm') = matchBAR(strm)
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (BAR2_RES, BAR2_SPAN, strm') = matchBAR(strm')
            val FULL_SPAN = (#1(BAR1_SPAN), #2(BAR2_SPAN))
            in
              (UserCode.AtomExpr_PROD_15_ACT (BAR1_RES, BAR2_RES, Expr_RES, BAR1_SPAN : (Lex.pos * Lex.pos), BAR2_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.BAR, _, strm') => AtomExpr_PROD_15(strm)
          | (Tok.KW_true, _, strm') => AtomExpr_PROD_13(strm)
          | (Tok.REAL(_), _, strm') => AtomExpr_PROD_11(strm)
          | (Tok.LB, _, strm') => AtomExpr_PROD_9(strm)
          | (Tok.LCB, _, strm') =>
              (case (lex(strm'))
               of (Tok.RCB, _, strm') => AtomExpr_PROD_7(strm)
                | (Tok.KW_false, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.KW_identity, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.KW_image, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.KW_load, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.KW_real, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.KW_true, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.KW_zeros, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.OP_minus, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.OP_D, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.OP_Dotimes, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.OP_curl, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.OP_Ddot, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.LP, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.LB, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.LCB, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.BANG, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.BAR, _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.ID(_), _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.INT(_), _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.REAL(_), _, strm') => AtomExpr_PROD_8(strm)
                | (Tok.STRING(_), _, strm') => AtomExpr_PROD_8(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_image, _, strm') => AtomExpr_PROD_5(strm)
          | (Tok.KW_load, _, strm') => AtomExpr_PROD_5(strm)
          | (Tok.KW_zeros, _, strm') => AtomExpr_PROD_3(strm)
          | (Tok.ID(_), _, strm') => AtomExpr_PROD_1(strm)
          | (Tok.KW_identity, _, strm') => AtomExpr_PROD_2(strm)
          | (Tok.KW_real, _, strm') => AtomExpr_PROD_4(strm)
          | (Tok.LP, _, strm') => AtomExpr_PROD_6(strm)
          | (Tok.INT(_), _, strm') => AtomExpr_PROD_10(strm)
          | (Tok.STRING(_), _, strm') => AtomExpr_PROD_12(strm)
          | (Tok.KW_false, _, strm') => AtomExpr_PROD_14(strm)
          | _ => fail()
        (* end case *))
      end
fun Iteration_NT (strm) = let
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
      val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
      val (Expr1_RES, Expr1_SPAN, strm') = Expr_NT(strm')
      val (DOTDOT_RES, DOTDOT_SPAN, strm') = matchDOTDOT(strm')
      val (Expr2_RES, Expr2_SPAN, strm') = Expr_NT(strm')
      val FULL_SPAN = (#1(BindId_SPAN), #2(Expr2_SPAN))
      in
        (UserCode.Iteration_PROD_1_ACT (Expr1_RES, Expr2_RES, KW_in_RES, BindId_RES, DOTDOT_RES, Expr1_SPAN : (Lex.pos * Lex.pos), Expr2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), DOTDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Iterations_NT (strm) = let
      val (Iteration_RES, Iteration_SPAN, strm') = Iteration_NT(strm)
      fun Iterations_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Iteration_RES, Iteration_SPAN, strm') = Iteration_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Iteration_SPAN))
            in
              ((Iteration_RES), FULL_SPAN, strm')
            end
      fun Iterations_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Iterations_PROD_1_SUBRULE_1_PRED, Iterations_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(Iteration_SPAN), #2(SR_SPAN))
      in
        (UserCode.Iterations_PROD_1_ACT (SR_RES, Iteration_RES, SR_SPAN : (Lex.pos * Lex.pos), Iteration_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Create_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (Arguments_RES, Arguments_SPAN, strm') = Arguments_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val FULL_SPAN = (#1(ID_SPAN), #2(RP_SPAN))
      in
        (UserCode.Create_PROD_1_ACT (ID_RES, LP_RES, RP_RES, Arguments_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Collection_NT (strm) = let
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
      val (Create_RES, Create_SPAN, strm') = Create_NT(strm')
      val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm')
      val (Iterations_RES, Iterations_SPAN, strm') = Iterations_NT(strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
      in
        (UserCode.Collection_PROD_1_ACT (BAR_RES, LCB_RES, RCB_RES, Iterations_RES, Create_RES, BAR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Iterations_SPAN : (Lex.pos * Lex.pos), Create_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Array_NT (strm) = let
      val (LB_RES, LB_SPAN, strm') = matchLB(strm)
      val (Create_RES, Create_SPAN, strm') = Create_NT(strm')
      val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm')
      val (Iterations_RES, Iterations_SPAN, strm') = Iterations_NT(strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
      in
        (UserCode.Array_PROD_1_ACT (LB_RES, RB_RES, BAR_RES, Iterations_RES, Create_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), BAR_SPAN : (Lex.pos * Lex.pos), Iterations_SPAN : (Lex.pos * Lex.pos), Create_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun CoordinationDecl_NT (strm) = let
      val (KW_initially_RES, KW_initially_SPAN, strm') = matchKW_initially(strm)
      val (SR_RES, SR_SPAN, strm') = let
      fun CoordinationDecl_PROD_1_SUBRULE_1_NT (strm) = let
            fun CoordinationDecl_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (Array_RES, Array_SPAN, strm') = Array_NT(strm)
                  val FULL_SPAN = (#1(Array_SPAN), #2(Array_SPAN))
                  in
                    ((Array_RES), FULL_SPAN, strm')
                  end
            fun CoordinationDecl_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (Collection_RES, Collection_SPAN, strm') = Collection_NT(strm)
                  val FULL_SPAN = (#1(Collection_SPAN), #2(Collection_SPAN))
                  in
                    ((Collection_RES), FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.LCB, _, strm') =>
                    CoordinationDecl_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.LB, _, strm') =>
                    CoordinationDecl_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      in
        CoordinationDecl_PROD_1_SUBRULE_1_NT(strm')
      end
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(KW_initially_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.CoordinationDecl_PROD_1_ACT (SR_RES, SEMI_RES, KW_initially_RES, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_initially_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun SeqDimensions_NT (strm) = let
      fun SeqDimensions_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.SeqDimensions_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun SeqDimensions_PROD_2 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.SeqDimensions_PROD_2_ACT (LCB_RES, RCB_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SeqDimensions_PROD_3 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val (SeqDimensions_RES, SeqDimensions_SPAN, strm') = SeqDimensions_NT(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(SeqDimensions_SPAN))
            in
              (UserCode.SeqDimensions_PROD_3_ACT (LCB_RES, RCB_RES, Dimension_RES, SeqDimensions_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), SeqDimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => SeqDimensions_PROD_1(strm)
          | (Tok.LCB, _, strm') =>
              (case (lex(strm'))
               of (Tok.RCB, _, strm') => SeqDimensions_PROD_2(strm)
                | (Tok.INT(_), _, strm') => SeqDimensions_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun ValueType_NT (strm) = let
      fun ValueType_PROD_1 (strm) = let
            val (KW_tensor_RES, KW_tensor_SPAN, strm') = matchKW_tensor(strm)
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_tensor_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.ValueType_PROD_1_ACT (Dimensions_RES, KW_tensor_RES, Dimensions_SPAN : (Lex.pos * Lex.pos), KW_tensor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_2 (strm) = let
            val (KW_vec2_RES, KW_vec2_SPAN, strm') = matchKW_vec2(strm)
            val FULL_SPAN = (#1(KW_vec2_SPAN), #2(KW_vec2_SPAN))
            in
              (UserCode.ValueType_PROD_2_ACT (KW_vec2_RES, KW_vec2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_3 (strm) = let
            val (KW_vec3_RES, KW_vec3_SPAN, strm') = matchKW_vec3(strm)
            val FULL_SPAN = (#1(KW_vec3_SPAN), #2(KW_vec3_SPAN))
            in
              (UserCode.ValueType_PROD_3_ACT (KW_vec3_RES, KW_vec3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_4 (strm) = let
            val (KW_vec4_RES, KW_vec4_SPAN, strm') = matchKW_vec4(strm)
            val FULL_SPAN = (#1(KW_vec4_SPAN), #2(KW_vec4_SPAN))
            in
              (UserCode.ValueType_PROD_4_ACT (KW_vec4_RES, KW_vec4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_5 (strm) = let
            val (KW_bool_RES, KW_bool_SPAN, strm') = matchKW_bool(strm)
            val FULL_SPAN = (#1(KW_bool_SPAN), #2(KW_bool_SPAN))
            in
              (UserCode.ValueType_PROD_5_ACT (KW_bool_RES, KW_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_6 (strm) = let
            val (KW_int_RES, KW_int_SPAN, strm') = matchKW_int(strm)
            val FULL_SPAN = (#1(KW_int_SPAN), #2(KW_int_SPAN))
            in
              (UserCode.ValueType_PROD_6_ACT (KW_int_RES, KW_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_7 (strm) = let
            val (KW_real_RES, KW_real_SPAN, strm') = matchKW_real(strm)
            val FULL_SPAN = (#1(KW_real_SPAN), #2(KW_real_SPAN))
            in
              (UserCode.ValueType_PROD_7_ACT (KW_real_RES, KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_8 (strm) = let
            val (KW_string_RES, KW_string_SPAN, strm') = matchKW_string(strm)
            val FULL_SPAN = (#1(KW_string_SPAN), #2(KW_string_SPAN))
            in
              (UserCode.ValueType_PROD_8_ACT (KW_string_RES, KW_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueType_PROD_9 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.ValueType_PROD_9_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => ValueType_PROD_9(strm)
          | (Tok.KW_real, _, strm') => ValueType_PROD_7(strm)
          | (Tok.KW_bool, _, strm') => ValueType_PROD_5(strm)
          | (Tok.KW_vec3, _, strm') => ValueType_PROD_3(strm)
          | (Tok.KW_tensor, _, strm') => ValueType_PROD_1(strm)
          | (Tok.KW_vec2, _, strm') => ValueType_PROD_2(strm)
          | (Tok.KW_vec4, _, strm') => ValueType_PROD_4(strm)
          | (Tok.KW_int, _, strm') => ValueType_PROD_6(strm)
          | (Tok.KW_string, _, strm') => ValueType_PROD_8(strm)
          | _ => fail()
        (* end case *))
      end
fun ConcreteType_NT (strm) = let
      val (ValueType_RES, ValueType_SPAN, strm') = ValueType_NT(strm)
      val (SeqDimensions_RES, SeqDimensions_SPAN, strm') = SeqDimensions_NT(strm')
      val FULL_SPAN = (#1(ValueType_SPAN), #2(SeqDimensions_SPAN))
      in
        (UserCode.ConcreteType_PROD_1_ACT (ValueType_RES, SeqDimensions_RES, ValueType_SPAN : (Lex.pos * Lex.pos), SeqDimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Type_NT (strm) = let
      fun Type_PROD_1 (strm) = let
            val (KW_image_RES, KW_image_SPAN, strm') = matchKW_image(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_image_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.Type_PROD_1_ACT (LP_RES, RP_RES, KW_image_RES, Dimensions_RES, Dimension_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_2 (strm) = let
            val (KW_field_RES, KW_field_SPAN, strm') = matchKW_field(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_field_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.Type_PROD_2_ACT (LP_RES, RP_RES, INT_RES, HASH_RES, KW_field_RES, Dimensions_RES, Dimension_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_field_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_3 (strm) = let
            val (KW_OField_RES, KW_OField_SPAN, strm') = matchKW_OField(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_OField_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.Type_PROD_3_ACT (LP_RES, RP_RES, INT_RES, HASH_RES, Dimensions_RES, KW_OField_RES, Dimension_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), KW_OField_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_4 (strm) = let
            val (KW_fem_RES, KW_fem_SPAN, strm') = matchKW_fem(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_fem_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.Type_PROD_4_ACT (LP_RES, RP_RES, INT_RES, HASH_RES, Dimensions_RES, Dimension_RES, KW_fem_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), KW_fem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_5 (strm) = let
            val (KW_kernel_RES, KW_kernel_SPAN, strm') = matchKW_kernel(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val FULL_SPAN = (#1(KW_kernel_SPAN), #2(INT_SPAN))
            in
              (UserCode.Type_PROD_5_ACT (INT_RES, HASH_RES, KW_kernel_RES, INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), KW_kernel_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_6 (strm) = let
            val (ConcreteType_RES, ConcreteType_SPAN, strm') = ConcreteType_NT(strm)
            val FULL_SPAN = (#1(ConcreteType_SPAN), #2(ConcreteType_SPAN))
            in
              ((ConcreteType_RES), FULL_SPAN, strm')
            end
      fun Type_PROD_7 (strm) = let
            val (KW_mesh_RES, KW_mesh_SPAN, strm') = matchKW_mesh(strm)
            val FULL_SPAN = (#1(KW_mesh_SPAN), #2(KW_mesh_SPAN))
            in
              (UserCode.Type_PROD_7_ACT (KW_mesh_RES, KW_mesh_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_8 (strm) = let
            val (KW_element_RES, KW_element_SPAN, strm') = matchKW_element(strm)
            val FULL_SPAN = (#1(KW_element_SPAN), #2(KW_element_SPAN))
            in
              (UserCode.Type_PROD_8_ACT (KW_element_RES, KW_element_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_9 (strm) = let
            val (KW_fnspace_RES, KW_fnspace_SPAN, strm') = matchKW_fnspace(strm)
            val FULL_SPAN = (#1(KW_fnspace_SPAN), #2(KW_fnspace_SPAN))
            in
              (UserCode.Type_PROD_9_ACT (KW_fnspace_RES, KW_fnspace_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fnspace, _, strm') => Type_PROD_9(strm)
          | (Tok.KW_mesh, _, strm') => Type_PROD_7(strm)
          | (Tok.KW_kernel, _, strm') => Type_PROD_5(strm)
          | (Tok.KW_OField, _, strm') => Type_PROD_3(strm)
          | (Tok.KW_image, _, strm') => Type_PROD_1(strm)
          | (Tok.KW_field, _, strm') => Type_PROD_2(strm)
          | (Tok.KW_fem, _, strm') => Type_PROD_4(strm)
          | (Tok.KW_bool, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_int, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_real, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_string, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_tensor, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_vec2, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_vec3, _, strm') => Type_PROD_6(strm)
          | (Tok.KW_vec4, _, strm') => Type_PROD_6(strm)
          | (Tok.ID(_), _, strm') => Type_PROD_6(strm)
          | (Tok.KW_element, _, strm') => Type_PROD_8(strm)
          | _ => fail()
        (* end case *))
      end
fun VarDecl_NT (strm) = let
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm)
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm')
      val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(Type_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.VarDecl_PROD_1_ACT (Expr_RES, SEMI_RES, Type_RES, OP_eq_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Block_NT (strm) = let
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
      fun Block_PROD_1_SUBRULE_1_NT (strm) = let
            val (Stmt_RES, Stmt_SPAN, strm') = Stmt_NT(strm)
            val FULL_SPAN = (#1(Stmt_SPAN), #2(Stmt_SPAN))
            in
              ((Stmt_RES), FULL_SPAN, strm')
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
      val (Stmt_RES, Stmt_SPAN, strm') = EBNF.closure(Block_PROD_1_SUBRULE_1_PRED, Block_PROD_1_SUBRULE_1_NT, strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
      in
        (UserCode.Block_PROD_1_ACT (LCB_RES, RCB_RES, Stmt_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Stmt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Stmt_NT (strm) = let
      fun Stmt_PROD_1 (strm) = let
            val (AtomicStmt_RES, AtomicStmt_SPAN, strm') = AtomicStmt_NT(strm)
            val FULL_SPAN = (#1(AtomicStmt_SPAN), #2(AtomicStmt_SPAN))
            in
              (UserCode.Stmt_PROD_1_ACT (AtomicStmt_RES, AtomicStmt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Stmt_PROD_2 (strm) = let
            val (KW_foreach_RES, KW_foreach_SPAN, strm') = matchKW_foreach(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val (Iterator_RES, Iterator_SPAN, strm') = Iterator_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Stmt_RES, Stmt_SPAN, strm') = Stmt_NT(strm')
            val FULL_SPAN = (#1(KW_foreach_SPAN), #2(Stmt_SPAN))
            in
              (UserCode.Stmt_PROD_2_ACT (LP_RES, RP_RES, Stmt_RES, Type_RES, KW_foreach_RES, Iterator_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Stmt_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_foreach_SPAN : (Lex.pos * Lex.pos), Iterator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Stmt_PROD_3 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Stmt1_RES, Stmt1_SPAN, strm') = Stmt_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (Stmt2_RES, Stmt2_SPAN, strm') = Stmt_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(Stmt2_SPAN))
            in
              (UserCode.Stmt_PROD_3_ACT (LP_RES, RP_RES, Expr_RES, KW_else_RES, KW_if_RES, Stmt1_RES, Stmt2_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), Stmt1_SPAN : (Lex.pos * Lex.pos), Stmt2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Stmt_PROD_4 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Stmt_RES, Stmt_SPAN, strm') = Stmt_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(Stmt_SPAN))
            in
              (UserCode.Stmt_PROD_4_ACT (LP_RES, RP_RES, Expr_RES, Stmt_RES, KW_if_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), Stmt_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_foreach, _, strm') => Stmt_PROD_2(strm)
          | (Tok.KW_bool, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_continue, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_die, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_field, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_image, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_int, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_kernel, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_new, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_print, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_real, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_return, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_stabilize, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_string, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_tensor, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_vec2, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_vec3, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_vec4, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_OField, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_fem, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_mesh, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_element, _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_fnspace, _, strm') => Stmt_PROD_1(strm)
          | (Tok.LCB, _, strm') => Stmt_PROD_1(strm)
          | (Tok.ID(_), _, strm') => Stmt_PROD_1(strm)
          | (Tok.KW_if, _, strm') => tryProds(strm, [Stmt_PROD_3, Stmt_PROD_4])
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
            val (VarDecl_RES, VarDecl_SPAN, strm') = VarDecl_NT(strm)
            val FULL_SPAN = (#1(VarDecl_SPAN), #2(VarDecl_SPAN))
            in
              (UserCode.AtomicStmt_PROD_2_ACT (VarDecl_RES, VarDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_3 (strm) = let
            val (KW_stabilize_RES, KW_stabilize_SPAN, strm') = matchKW_stabilize(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_stabilize_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_3_ACT (SEMI_RES, KW_stabilize_RES, SEMI_SPAN : (Lex.pos * Lex.pos), KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_4 (strm) = let
            val (KW_continue_RES, KW_continue_SPAN, strm') = matchKW_continue(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_continue_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_4_ACT (SEMI_RES, KW_continue_RES, SEMI_SPAN : (Lex.pos * Lex.pos), KW_continue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
            val (KW_new_RES, KW_new_SPAN, strm') = matchKW_new(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Arguments_RES, Arguments_SPAN, strm') = Arguments_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_new_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_6_ACT (ID_RES, LP_RES, RP_RES, SEMI_RES, Arguments_RES, KW_new_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Arguments_SPAN : (Lex.pos * Lex.pos), KW_new_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_7 (strm) = let
            val (KW_print_RES, KW_print_SPAN, strm') = matchKW_print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            fun AtomicStmt_PROD_7_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Expr_SPAN))
                  in
                    ((Expr_RES), FULL_SPAN, strm')
                  end
            fun AtomicStmt_PROD_7_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicStmt_PROD_7_SUBRULE_1_PRED, AtomicStmt_PROD_7_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_print_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_7_ACT (LP_RES, RP_RES, SR_RES, Expr_RES, SEMI_RES, KW_print_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_print_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_8 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_8_ACT (Expr_RES, SEMI_RES, OP_eq_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_9 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (OP_pluseq_RES, OP_pluseq_SPAN, strm') = matchOP_pluseq(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_9_ACT (Expr_RES, SEMI_RES, OP_pluseq_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_pluseq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_10 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (OP_minuseq_RES, OP_minuseq_SPAN, strm') = matchOP_minuseq(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_10_ACT (Expr_RES, SEMI_RES, OP_minuseq_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_minuseq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_11 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (OP_stareq_RES, OP_stareq_SPAN, strm') = matchOP_stareq(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_11_ACT (Expr_RES, SEMI_RES, OP_stareq_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_stareq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_12 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (OP_slasheq_RES, OP_slasheq_SPAN, strm') = matchOP_slasheq(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_12_ACT (Expr_RES, SEMI_RES, OP_slasheq_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_slasheq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_13 (strm) = let
            val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm)
            val (OP_modeq_RES, OP_modeq_SPAN, strm') = matchOP_modeq(strm')
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(BindId_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_13_ACT (Expr_RES, SEMI_RES, OP_modeq_RES, BindId_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_modeq_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicStmt_PROD_14 (strm) = let
            val (KW_return_RES, KW_return_SPAN, strm') = matchKW_return(strm)
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_return_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.AtomicStmt_PROD_14_ACT (Expr_RES, SEMI_RES, KW_return_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_return_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_return, _, strm') => AtomicStmt_PROD_14(strm)
          | (Tok.KW_new, _, strm') => AtomicStmt_PROD_6(strm)
          | (Tok.KW_continue, _, strm') => AtomicStmt_PROD_4(strm)
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.LCB, _, strm') => AtomicStmt_PROD_2(strm)
                | (Tok.ID(_), _, strm') => AtomicStmt_PROD_2(strm)
                | (Tok.OP_pluseq, _, strm') => AtomicStmt_PROD_9(strm)
                | (Tok.OP_stareq, _, strm') => AtomicStmt_PROD_11(strm)
                | (Tok.OP_modeq, _, strm') => AtomicStmt_PROD_13(strm)
                | (Tok.OP_slasheq, _, strm') => AtomicStmt_PROD_12(strm)
                | (Tok.OP_minuseq, _, strm') => AtomicStmt_PROD_10(strm)
                | (Tok.OP_eq, _, strm') => AtomicStmt_PROD_8(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_bool, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_field, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_image, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_int, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_kernel, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_real, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_string, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_tensor, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_vec2, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_vec3, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_vec4, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_OField, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_fem, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_mesh, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_element, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.KW_fnspace, _, strm') => AtomicStmt_PROD_2(strm)
          | (Tok.LCB, _, strm') => AtomicStmt_PROD_1(strm)
          | (Tok.KW_stabilize, _, strm') => AtomicStmt_PROD_3(strm)
          | (Tok.KW_die, _, strm') => AtomicStmt_PROD_5(strm)
          | (Tok.KW_print, _, strm') => AtomicStmt_PROD_7(strm)
          | _ => fail()
        (* end case *))
      end
fun GlobalUpdate_NT (strm) = let
      val (KW_global_RES, KW_global_SPAN, strm') = matchKW_global(strm)
      val (Block_RES, Block_SPAN, strm') = Block_NT(strm')
      val FULL_SPAN = (#1(KW_global_SPAN), #2(Block_SPAN))
      in
        (UserCode.GlobalUpdate_PROD_1_ACT (Block_RES, KW_global_RES, Block_SPAN : (Lex.pos * Lex.pos), KW_global_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun MethodId_NT (strm) = let
      fun MethodId_PROD_1 (strm) = let
            val (KW_update_RES, KW_update_SPAN, strm') = matchKW_update(strm)
            val FULL_SPAN = (#1(KW_update_SPAN), #2(KW_update_SPAN))
            in
              (UserCode.MethodId_PROD_1_ACT (KW_update_RES, KW_update_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MethodId_PROD_2 (strm) = let
            val (KW_stabilize_RES, KW_stabilize_SPAN, strm') = matchKW_stabilize(strm)
            val FULL_SPAN = (#1(KW_stabilize_SPAN), #2(KW_stabilize_SPAN))
            in
              (UserCode.MethodId_PROD_2_ACT (KW_stabilize_RES, KW_stabilize_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_stabilize, _, strm') => MethodId_PROD_2(strm)
          | (Tok.KW_update, _, strm') => MethodId_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun StrandMethod_NT (strm) = let
      val (MethodId_RES, MethodId_SPAN, strm') = MethodId_NT(strm)
      val (Block_RES, Block_SPAN, strm') = Block_NT(strm')
      val FULL_SPAN = (#1(MethodId_SPAN), #2(Block_SPAN))
      in
        (UserCode.StrandMethod_PROD_1_ACT (Block_RES, MethodId_RES, Block_SPAN : (Lex.pos * Lex.pos), MethodId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun StrandStateDecl_NT (strm) = let
      fun StrandStateDecl_PROD_1 (strm) = let
            val (KW_output_RES, KW_output_SPAN, strm') = matchKW_output(strm)
            val (VarDecl_RES, VarDecl_SPAN, strm') = VarDecl_NT(strm')
            val FULL_SPAN = (#1(KW_output_SPAN), #2(VarDecl_SPAN))
            in
              (UserCode.StrandStateDecl_PROD_1_ACT (VarDecl_RES, KW_output_RES, VarDecl_SPAN : (Lex.pos * Lex.pos), KW_output_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun StrandStateDecl_PROD_2 (strm) = let
            val (VarDecl_RES, VarDecl_SPAN, strm') = VarDecl_NT(strm)
            val FULL_SPAN = (#1(VarDecl_SPAN), #2(VarDecl_SPAN))
            in
              (UserCode.StrandStateDecl_PROD_2_ACT (VarDecl_RES, VarDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_bool, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_field, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_image, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_int, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_kernel, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_real, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_string, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_tensor, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_vec2, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_vec3, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_vec4, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_OField, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_fem, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_mesh, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_element, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_fnspace, _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.ID(_), _, strm') => StrandStateDecl_PROD_2(strm)
          | (Tok.KW_output, _, strm') => StrandStateDecl_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Param_NT (strm) = let
      val (ValueType_RES, ValueType_SPAN, strm') = ValueType_NT(strm)
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      val FULL_SPAN = (#1(ValueType_SPAN), #2(BindId_SPAN))
      in
        (UserCode.Param_PROD_1_ACT (ValueType_RES, BindId_RES, ValueType_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Params_NT (strm) = let
      fun Params_PROD_1_SUBRULE_1_NT (strm) = let
            val (Param_RES, Param_SPAN, strm') = Param_NT(strm)
            fun Params_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Param_RES, Param_SPAN, strm') = Param_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Param_SPAN))
                  in
                    ((Param_RES), FULL_SPAN, strm')
                  end
            fun Params_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Params_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, Params_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Param_SPAN), #2(SR_SPAN))
            in
              ((Param_RES, SR_RES), FULL_SPAN, strm')
            end
      fun Params_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_bool, _, strm') => true
              | (Tok.KW_int, _, strm') => true
              | (Tok.KW_real, _, strm') => true
              | (Tok.KW_string, _, strm') => true
              | (Tok.KW_tensor, _, strm') => true
              | (Tok.KW_vec2, _, strm') => true
              | (Tok.KW_vec3, _, strm') => true
              | (Tok.KW_vec4, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Params_PROD_1_SUBRULE_1_PRED, Params_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.Params_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun StrandDecl_NT (strm) = let
      val (KW_strand_RES, KW_strand_SPAN, strm') = matchKW_strand(strm)
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (Params_RES, Params_SPAN, strm') = Params_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm')
      fun StrandDecl_PROD_1_SUBRULE_1_NT (strm) = let
            val (StrandStateDecl_RES, StrandStateDecl_SPAN, strm') = StrandStateDecl_NT(strm)
            val FULL_SPAN = (#1(StrandStateDecl_SPAN),
              #2(StrandStateDecl_SPAN))
            in
              ((StrandStateDecl_RES), FULL_SPAN, strm')
            end
      fun StrandDecl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_bool, _, strm') => true
              | (Tok.KW_field, _, strm') => true
              | (Tok.KW_image, _, strm') => true
              | (Tok.KW_int, _, strm') => true
              | (Tok.KW_kernel, _, strm') => true
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
      val (StrandStateDecl_RES, StrandStateDecl_SPAN, strm') = EBNF.closure(StrandDecl_PROD_1_SUBRULE_1_PRED, StrandDecl_PROD_1_SUBRULE_1_NT, strm')
      fun StrandDecl_PROD_1_SUBRULE_2_NT (strm) = let
            val (StrandMethod_RES, StrandMethod_SPAN, strm') = StrandMethod_NT(strm)
            val FULL_SPAN = (#1(StrandMethod_SPAN), #2(StrandMethod_SPAN))
            in
              ((StrandMethod_RES), FULL_SPAN, strm')
            end
      fun StrandDecl_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_stabilize, _, strm') => true
              | (Tok.KW_update, _, strm') => true
              | _ => false
            (* end case *))
      val (StrandMethod_RES, StrandMethod_SPAN, strm') = EBNF.posclos(StrandDecl_PROD_1_SUBRULE_2_PRED, StrandDecl_PROD_1_SUBRULE_2_NT, strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      val FULL_SPAN = (#1(KW_strand_SPAN), #2(RCB_SPAN))
      in
        (UserCode.StrandDecl_PROD_1_ACT (LP_RES, RP_RES, LCB_RES, RCB_RES, Params_RES, StrandMethod_RES, StrandStateDecl_RES, BindId_RES, KW_strand_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Params_SPAN : (Lex.pos * Lex.pos), StrandMethod_SPAN : (Lex.pos * Lex.pos), StrandStateDecl_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), KW_strand_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun FunBody_NT (strm) = let
      fun FunBody_PROD_1 (strm) = let
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm)
            val (Expr_RES, Expr_SPAN, strm') = Expr_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(OP_eq_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.FunBody_PROD_1_ACT (Expr_RES, SEMI_RES, OP_eq_RES, Expr_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), OP_eq_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun FunBody_PROD_2 (strm) = let
            val (Block_RES, Block_SPAN, strm') = Block_NT(strm)
            val FULL_SPAN = (#1(Block_SPAN), #2(Block_SPAN))
            in
              (UserCode.FunBody_PROD_2_ACT (Block_RES, Block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LCB, _, strm') => FunBody_PROD_2(strm)
          | (Tok.OP_eq, _, strm') => FunBody_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun FunDecl_NT (strm) = let
      val (KW_function_RES, KW_function_SPAN, strm') = matchKW_function(strm)
      val (ConcreteType_RES, ConcreteType_SPAN, strm') = ConcreteType_NT(strm')
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (Params_RES, Params_SPAN, strm') = Params_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val (FunBody_RES, FunBody_SPAN, strm') = FunBody_NT(strm')
      val FULL_SPAN = (#1(KW_function_SPAN), #2(FunBody_SPAN))
      in
        (UserCode.FunDecl_PROD_1_ACT (LP_RES, RP_RES, Params_RES, ConcreteType_RES, KW_function_RES, BindId_RES, FunBody_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Params_SPAN : (Lex.pos * Lex.pos), ConcreteType_SPAN : (Lex.pos * Lex.pos), KW_function_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FunBody_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Initializer_NT (strm) = let
      fun Initializer_PROD_1 (strm) = let
            val (KW_identity_RES, KW_identity_SPAN, strm') = matchKW_identity(strm)
            val (LB_RES, LB_SPAN, strm') = matchLB(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(KW_identity_SPAN), #2(RB_SPAN))
            in
              (UserCode.Initializer_PROD_1_ACT (LB_RES, RB_RES, KW_identity_RES, Dimension_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), KW_identity_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_2 (strm) = let
            val (KW_zeros_RES, KW_zeros_SPAN, strm') = matchKW_zeros(strm)
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_zeros_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.Initializer_PROD_2_ACT (KW_zeros_RES, Dimensions_RES, KW_zeros_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_3 (strm) = let
            val (LoadExpr_RES, LoadExpr_SPAN, strm') = LoadExpr_NT(strm)
            val FULL_SPAN = (#1(LoadExpr_SPAN), #2(LoadExpr_SPAN))
            in
              (UserCode.Initializer_PROD_3_ACT (LoadExpr_RES, LoadExpr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_4 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Initializer_RES, Initializer_SPAN, strm') = Initializer_NT(strm')
            fun Initializer_PROD_4_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Initializer_RES, Initializer_SPAN, strm') = Initializer_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Initializer_SPAN))
                  in
                    ((Initializer_RES), FULL_SPAN, strm')
                  end
            fun Initializer_PROD_4_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Initializer_PROD_4_SUBRULE_1_PRED, Initializer_PROD_4_SUBRULE_1_NT, strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.Initializer_PROD_4_ACT (SR_RES, LCB_RES, RCB_RES, Initializer_RES, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Initializer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_5 (strm) = let
            val (LB_RES, LB_SPAN, strm') = matchLB(strm)
            val (Initializer_RES, Initializer_SPAN, strm') = Initializer_NT(strm')
            fun Initializer_PROD_5_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Initializer_RES, Initializer_SPAN, strm') = Initializer_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Initializer_SPAN))
                  in
                    ((Initializer_RES), FULL_SPAN, strm')
                  end
            fun Initializer_PROD_5_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Initializer_PROD_5_SUBRULE_1_PRED, Initializer_PROD_5_SUBRULE_1_NT, strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
            in
              (UserCode.Initializer_PROD_5_ACT (LB_RES, RB_RES, SR_RES, Initializer_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Initializer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_6 (strm) = let
            val (OP_minus_RES, OP_minus_SPAN, strm') = matchOP_minus(strm)
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val FULL_SPAN = (#1(OP_minus_SPAN), #2(INT_SPAN))
            in
              (UserCode.Initializer_PROD_6_ACT (INT_RES, OP_minus_RES, INT_SPAN : (Lex.pos * Lex.pos), OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_7 (strm) = let
            val (OP_minus_RES, OP_minus_SPAN, strm') = matchOP_minus(strm)
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm')
            val FULL_SPAN = (#1(OP_minus_SPAN), #2(REAL_SPAN))
            in
              (UserCode.Initializer_PROD_7_ACT (REAL_RES, OP_minus_RES, REAL_SPAN : (Lex.pos * Lex.pos), OP_minus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_8 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.Initializer_PROD_8_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_9 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.Initializer_PROD_9_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_10 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.Initializer_PROD_10_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_11 (strm) = let
            val (KW_true_RES, KW_true_SPAN, strm') = matchKW_true(strm)
            val FULL_SPAN = (#1(KW_true_SPAN), #2(KW_true_SPAN))
            in
              (UserCode.Initializer_PROD_11_ACT (KW_true_RES, KW_true_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Initializer_PROD_12 (strm) = let
            val (KW_false_RES, KW_false_SPAN, strm') = matchKW_false(strm)
            val FULL_SPAN = (#1(KW_false_SPAN), #2(KW_false_SPAN))
            in
              (UserCode.Initializer_PROD_12_ACT (KW_false_RES, KW_false_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_false, _, strm') => Initializer_PROD_12(strm)
          | (Tok.STRING(_), _, strm') => Initializer_PROD_10(strm)
          | (Tok.INT(_), _, strm') => Initializer_PROD_8(strm)
          | (Tok.OP_minus, _, strm') =>
              (case (lex(strm'))
               of (Tok.INT(_), _, strm') => Initializer_PROD_6(strm)
                | (Tok.REAL(_), _, strm') => Initializer_PROD_7(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LCB, _, strm') => Initializer_PROD_4(strm)
          | (Tok.KW_zeros, _, strm') => Initializer_PROD_2(strm)
          | (Tok.KW_identity, _, strm') => Initializer_PROD_1(strm)
          | (Tok.KW_image, _, strm') => Initializer_PROD_3(strm)
          | (Tok.KW_load, _, strm') => Initializer_PROD_3(strm)
          | (Tok.LB, _, strm') => Initializer_PROD_5(strm)
          | (Tok.REAL(_), _, strm') => Initializer_PROD_9(strm)
          | (Tok.KW_true, _, strm') => Initializer_PROD_11(strm)
          | _ => fail()
        (* end case *))
      end
fun InputType_NT (strm) = let
      fun InputType_PROD_1 (strm) = let
            val (KW_image_RES, KW_image_SPAN, strm') = matchKW_image(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_image_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.InputType_PROD_1_ACT (LP_RES, RP_RES, KW_image_RES, Dimensions_RES, Dimension_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_image_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun InputType_PROD_2 (strm) = let
            val (KW_fem_RES, KW_fem_SPAN, strm') = matchKW_fem(strm)
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm')
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (Dimension_RES, Dimension_SPAN, strm') = Dimension_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (Dimensions_RES, Dimensions_SPAN, strm') = Dimensions_NT(strm')
            val FULL_SPAN = (#1(KW_fem_SPAN), #2(Dimensions_SPAN))
            in
              (UserCode.InputType_PROD_2_ACT (LP_RES, RP_RES, INT_RES, HASH_RES, Dimensions_RES, Dimension_RES, KW_fem_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), HASH_SPAN : (Lex.pos * Lex.pos), Dimensions_SPAN : (Lex.pos * Lex.pos), Dimension_SPAN : (Lex.pos * Lex.pos), KW_fem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun InputType_PROD_3 (strm) = let
            val (ValueType_RES, ValueType_SPAN, strm') = ValueType_NT(strm)
            val (SeqDimensions_RES, SeqDimensions_SPAN, strm') = SeqDimensions_NT(strm')
            val FULL_SPAN = (#1(ValueType_SPAN), #2(SeqDimensions_SPAN))
            in
              (UserCode.InputType_PROD_3_ACT (ValueType_RES, SeqDimensions_RES, ValueType_SPAN : (Lex.pos * Lex.pos), SeqDimensions_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_bool, _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_int, _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_real, _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_string, _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_tensor, _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_vec2, _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_vec3, _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_vec4, _, strm') => InputType_PROD_3(strm)
          | (Tok.ID(_), _, strm') => InputType_PROD_3(strm)
          | (Tok.KW_image, _, strm') => InputType_PROD_1(strm)
          | (Tok.KW_fem, _, strm') => InputType_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun InputDecl_NT (strm) = let
      val (KW_input_RES, KW_input_SPAN, strm') = matchKW_input(strm)
      val (InputType_RES, InputType_SPAN, strm') = InputType_NT(strm')
      val (BindId_RES, BindId_SPAN, strm') = BindId_NT(strm')
      fun InputDecl_PROD_1_SUBRULE_1_NT (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((STRING_RES), FULL_SPAN, strm')
            end
      fun InputDecl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LP, _, strm') => true
              | _ => false
            (* end case *))
      val (SR1_RES, SR1_SPAN, strm') = EBNF.optional(InputDecl_PROD_1_SUBRULE_1_PRED, InputDecl_PROD_1_SUBRULE_1_NT, strm')
      fun InputDecl_PROD_1_SUBRULE_2_NT (strm) = let
            val (OP_eq_RES, OP_eq_SPAN, strm') = matchOP_eq(strm)
            val (Initializer_RES, Initializer_SPAN, strm') = Initializer_NT(strm')
            val FULL_SPAN = (#1(OP_eq_SPAN), #2(Initializer_SPAN))
            in
              ((Initializer_RES), FULL_SPAN, strm')
            end
      fun InputDecl_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.OP_eq, _, strm') => true
              | _ => false
            (* end case *))
      val (SR2_RES, SR2_SPAN, strm') = EBNF.optional(InputDecl_PROD_1_SUBRULE_2_PRED, InputDecl_PROD_1_SUBRULE_2_NT, strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(KW_input_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.InputDecl_PROD_1_ACT (SR1_RES, SR2_RES, SEMI_RES, KW_input_RES, InputType_RES, BindId_RES, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_input_SPAN : (Lex.pos * Lex.pos), InputType_SPAN : (Lex.pos * Lex.pos), BindId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun GlobalDecl_NT (strm) = let
      fun GlobalDecl_PROD_1 (strm) = let
            val (InputDecl_RES, InputDecl_SPAN, strm') = InputDecl_NT(strm)
            val FULL_SPAN = (#1(InputDecl_SPAN), #2(InputDecl_SPAN))
            in
              ((InputDecl_RES), FULL_SPAN, strm')
            end
      fun GlobalDecl_PROD_2 (strm) = let
            val (VarDecl_RES, VarDecl_SPAN, strm') = VarDecl_NT(strm)
            val FULL_SPAN = (#1(VarDecl_SPAN), #2(VarDecl_SPAN))
            in
              (UserCode.GlobalDecl_PROD_2_ACT (VarDecl_RES, VarDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun GlobalDecl_PROD_3 (strm) = let
            val (FunDecl_RES, FunDecl_SPAN, strm') = FunDecl_NT(strm)
            val FULL_SPAN = (#1(FunDecl_SPAN), #2(FunDecl_SPAN))
            in
              ((FunDecl_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_function, _, strm') => GlobalDecl_PROD_3(strm)
          | (Tok.KW_input, _, strm') => GlobalDecl_PROD_1(strm)
          | (Tok.KW_bool, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_field, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_image, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_int, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_kernel, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_real, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_string, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_tensor, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_vec2, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_vec3, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_vec4, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_OField, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_fem, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_mesh, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_element, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.KW_fnspace, _, strm') => GlobalDecl_PROD_2(strm)
          | (Tok.ID(_), _, strm') => GlobalDecl_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun Program_NT (strm) = let
      fun Program_PROD_1_SUBRULE_1_NT (strm) = let
            val (GlobalDecl_RES, GlobalDecl_SPAN, strm') = GlobalDecl_NT(strm)
            val FULL_SPAN = (#1(GlobalDecl_SPAN), #2(GlobalDecl_SPAN))
            in
              ((GlobalDecl_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_bool, _, strm') => true
              | (Tok.KW_field, _, strm') => true
              | (Tok.KW_function, _, strm') => true
              | (Tok.KW_image, _, strm') => true
              | (Tok.KW_input, _, strm') => true
              | (Tok.KW_int, _, strm') => true
              | (Tok.KW_kernel, _, strm') => true
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
      val (GlobalDecl_RES, GlobalDecl_SPAN, strm') = EBNF.closure(Program_PROD_1_SUBRULE_1_PRED, Program_PROD_1_SUBRULE_1_NT, strm)
      val (StrandDecl_RES, StrandDecl_SPAN, strm') = StrandDecl_NT(strm')
      fun Program_PROD_1_SUBRULE_2_NT (strm) = let
            val (GlobalUpdate_RES, GlobalUpdate_SPAN, strm') = GlobalUpdate_NT(strm)
            val FULL_SPAN = (#1(GlobalUpdate_SPAN), #2(GlobalUpdate_SPAN))
            in
              ((GlobalUpdate_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_global, _, strm') => true
              | _ => false
            (* end case *))
      val (GlobalUpdate_RES, GlobalUpdate_SPAN, strm') = EBNF.optional(Program_PROD_1_SUBRULE_2_PRED, Program_PROD_1_SUBRULE_2_NT, strm')
      val (CoordinationDecl_RES, CoordinationDecl_SPAN, strm') = CoordinationDecl_NT(strm')
      val FULL_SPAN = (#1(GlobalDecl_SPAN), #2(CoordinationDecl_SPAN))
      in
        (UserCode.Program_PROD_1_ACT (StrandDecl_RES, CoordinationDecl_RES, GlobalDecl_RES, GlobalUpdate_RES, StrandDecl_SPAN : (Lex.pos * Lex.pos), CoordinationDecl_SPAN : (Lex.pos * Lex.pos), GlobalDecl_SPAN : (Lex.pos * Lex.pos), GlobalUpdate_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Root_NT (strm) = let
      fun Root_PROD_1_SUBRULE_1_NT (strm) = let
            val (VERSION_RES, VERSION_SPAN, strm') = matchVERSION(strm)
            val FULL_SPAN = (#1(VERSION_SPAN), #2(VERSION_SPAN))
            in
              ((VERSION_RES), FULL_SPAN, strm')
            end
      fun Root_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.VERSION(_), _, strm') => true
              | _ => false
            (* end case *))
      val (VERSION_RES, VERSION_SPAN, strm') = EBNF.optional(Root_PROD_1_SUBRULE_1_PRED, Root_PROD_1_SUBRULE_1_NT, strm)
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
