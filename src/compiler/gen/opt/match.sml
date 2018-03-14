(* match.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CompileMatch : sig

    type 'a state

    datatype path = PATH of int list

    datatype 'a state_kind
      = SWITCH of (path * (simple_pat * 'a state) list)    
      | FINAL of (path CheckSpec.VMap.map * 'a)
      | ERROR
(*Deleted test and when conditions*)

    and simple_pat
      = ANY
      | DECONS of (CheckSpec.oper * path list)
(*interface*)
    val same : ('a state * 'a state) -> bool
    val kind : 'a state -> 'a state_kind
    val stamp : 'a state -> word
    val refCnt : 'a state -> int
    val holder : 'a state -> PropList.holder
(*deleted ML.raw_ml option*)

    val compile : (CheckSpec.pattern * 'a) list -> 'a state

    structure PSet : ORD_SET where type Key.ord_key = path
    structure PMap : ORD_MAP where type Key.ord_key = path

  end = struct

(* original. 
    structure SP = SpecParser
    structure CS = CheckSpec
    structure S = Sorts
    structure Op = PrimOps
    structure VMap = CS.VMap
*)
    structure CS = CheckSpec (* rewrite rules*)
    structure VMap = CS.VMap

  (******************** Op and constants ********************)

    datatype con = OP of CS.oper (*| LIT of IntInf.int*)

    fun sameCon (OP c1, OP c2) = CS.sameOp(c1, c2)
(*
      | sameCon (LIT n1, LIT n2) = (n1 = n2)
      | sameCon _ = raise Fail "mixed operator/literal"
*)

    structure ConSet = RedBlackSetFn (
      struct
        type ord_key = con
        fun compare (OP c1, OP c2) = CS.compareOp(c1, c2)
(*
          | compare (LIT n1, LIT n2) = IntInf.compare(n1, n2)
          | compare _ = raise Fail "mixed operator/literal"
*)
      end)


  (******************** Paths and renamed patterns ********************)

    datatype path = PATH of int list

    fun extendPath (PATH l, i) = PATH(l @ [i])

  (* source patterns after variable renaming and operator lookup *)
    datatype pat
      = P_Wild
      | P_ConApp of (CS.oper * (path * pat) list)
(*
      | P_IConst of IntInf.int
*)

  (******************** DFA States ********************)

    type stamp = Word.word

    datatype 'a state = S of {
        refCnt : int ref,
        id : stamp,
        holder : PropList.holder,
        kind : 'a state_kind
      }

    and 'a state_kind
      = SWITCH of (path * (simple_pat * 'a state) list)
      | FINAL of (path CS.VMap.map * 'a)
      | ERROR

    and simple_pat
      = ANY
      | DECONS of (CS.oper * path list)

    local
      val idCnt = ref 0w0
    in
    fun mkState kind = let
          val id = !idCnt
          in
            idCnt := id+0w1;
            S{refCnt=ref 0, id=id, holder = PropList.newHolder(), kind=kind}
          end
    fun initState () = (idCnt := 0w0; mkState ERROR)
    end

  (* construct a final state *)
    fun finalState (vMap, exp) = mkState (FINAL(vMap, exp))

  (* construct a test state *)
    fun switchState (testVar, arcs) = mkState (SWITCH(testVar, arcs))

  (* increment a state's reference count *)
    fun inc (S{refCnt, ...}) = refCnt := !refCnt+1

  (* exported accessor functions on states *)
    fun same (S{refCnt=r1, ...}, S{refCnt=r2, ...}) = (r1 = r2)
    fun kind (S{kind, ...}) = kind
    fun stamp (S{id, ...}) = id
    fun refCnt (S{refCnt, ...}) = !refCnt
    fun holder (S{holder, ...}) = holder


  (******************** Pattern matrix ********************)
    datatype cell
      = NIL
      | CELL of {
          pat : pat,
          right : cell,
          down : cell
        }

    type 'a matrix = {
            rows : (cell * 'a state) list,
                                        (* cells of the first column with the *)
                                        (* optional "when" clause and the *)
                                        (* corresponding final state *)
            cols : cell list,           (* cells of the top row *)
            vars : path list            (* variables being tested (one per *)
                                        (* column *)
          }

    fun mkNilMat vars = {rows = [], cols = List.map (fn _ => NIL) vars, vars = vars}

    fun rowToList NIL = []
      | rowToList (cell as CELL{right, ...}) = cell :: rowToList right

  (* create a pattern matrix from a list of rows. *)
    fun mkMatrix (match as ((row1, _)::_)) = let
          val vars = map #1 row1
          fun mkRows [] = (List.map (fn _ => NIL) vars, [])
            | mkRows ((row, q)::rows) = let
                fun doCols ([], []) = NIL
                  | doCols ((_, pat)::r, cell::right) = CELL{
                        pat = pat, right = doCols (r, right), down = cell
                      }
                val (topRow, rows) = mkRows rows
                val newRow = doCols (row, topRow)
                in
                  (rowToList newRow, (newRow, q)::rows)
                end
          val (topRow, rows) = mkRows match
          in
            { rows = rows, cols = topRow, vars = vars }
          end

  (* choose a column of a matrix for splitting; currently we choose the column
   * with a constructor in its first row and the largest number of distinct
   * constructors.  If all the columns start with a variable, return NONE.
   *)
    fun chooseCol ({rows, cols, vars} : 'a matrix) = let
          fun count (NIL, cons) = ConSet.numItems cons
            | count (CELL{pat, down, ...}, cons) = let
                val cons = (case pat
                       of (P_ConApp(c, _)) => ConSet.add(cons, OP c)
(*
                        | (P_IConst n) => ConSet.add(cons, LIT n)
*)
                        | P_Wild => cons
                      (* end case *))
                in
                  count (down, cons)
                end
          fun maxRow (curMax, curCnt, _, []) = curMax
            | maxRow (curMax, curCnt, i, CELL{pat=P_Wild, ...}::cols) =
                maxRow (curMax, curCnt, i+1, cols)
            | maxRow (curMax, curCnt, i, col::cols) = let
                val cnt = count(col, ConSet.empty)
                in
                  if (cnt > curCnt)
                    then maxRow (SOME i, cnt, i+1, cols)
                    else maxRow (curMax, curCnt, i+1, cols)
                end
          in
            maxRow (NONE, 0, 0, cols)
          end

  (* add a row to the top of a matrix *)
    fun addRow ({rows, cols, vars}, (row, q)) = let
          fun cons (NIL, []) = (NIL, [])
            | cons (CELL{pat, right = r1, ...}, dn::r2) = let
                val (right, cols) = cons(r1, r2)
                val cell = CELL{pat = pat, right = right, down = dn}
                in
                  (cell, cell::cols)
                end
          val (row, cols) = cons (row, cols)
          in
            { rows = (row, q) :: rows, cols = cols, vars = vars }
          end

  (* replace the ith variable with newVars *)
    fun expandVars (vars, i, newVars) = let
          fun ins (0, _::r) = newVars @ r
            | ins (i, v::r) = v :: ins(i-1, r)
          in
            ins (i, vars)
          end

  (* replace the ith cell of a row with the expansion of args *)
    fun expandCols ((row, q), i, args) = let
          fun ins (0, CELL{right, ...}) = let
                fun cons [] = right
                  | cons ((_, pat)::r) = CELL{
                        pat = pat, down = NIL, right = cons r
                      }
                in
                  cons args
                end
            | ins (i, CELL{pat, right, ...}) = CELL{
                  pat = pat, down = NIL, right = ins (i-1, right)
                }
          in
            (ins (i, row), q)
          end

  (* Constructor map *)
    type 'a cons_info = {
        con : con,
        args : path list,
        mat : 'a matrix ref
      }


  (* split a pattern matrix based on the constructors of the given column.
   * For each constructor in the selected column, we construct a new pattern
   * matrix that contains a row for each row that matches the constructor.
   * This new matrix includes any rows where there is a variable in the selected
   * column.
   * Note that it is important that the order of constructors be preserved
   * and that the order of rows that have the same constructor also be preserved.
   *)
    fun splitAtCol (mat : 'a matrix, i) = let
          val vars = #vars mat
        (* find the entry for a constructor in the conMap *)
          fun findCon (conMap : 'a cons_info list, c) = let
                fun find [] = NONE
                  | find ({con, mat, args}::r) = if sameCon(c, con)
                      then SOME mat
                      else find r
                in
                  find conMap
                end
        (* create the initial conMap (one entry per constructor in the
         * column).
         *)
          fun mkConMap NIL = []
            | mkConMap (CELL{down, pat, ...}) = let
                val conMap = mkConMap down
                in
                  case pat
                   of P_Wild => conMap
                    | (P_ConApp(c, args)) => (case findCon(conMap, OP c)
                         of NONE => let
                              val argVars = map #1 args
                              val vars = expandVars(vars, i, argVars)
                              val mat = mkNilMat vars
                              in
                                {con = OP c, args = argVars, mat = ref mat} :: conMap
                              end
                          | (SOME _) => conMap
                        (* end case *))
(*
                    | (P_IConst n) => (case findCon(conMap, LIT n)
                         of NONE => let
                              val vars = expandVars(vars, i, [])
                              val mat = mkNilMat vars
                              in
                                {con = LIT n, args = [], mat = ref mat} :: conMap
                              end
                          | (SOME _) => conMap
                        (* end case *))
*)
                  (* end case *)
                end
          val splitCol = List.nth(#cols mat, i)
          val conMap = mkConMap splitCol
        (* populate the conMap and build the varMap *)
          fun f ([], _) = mkNilMat vars
            | f (row::rows, CELL{pat, right, down}) = let
                  val varMat = f (rows, down)
                  in
                    case pat
                     of P_Wild => let
                          fun addVarRow {con, args, mat} =
                                mat := addRow(!mat,
                                  expandCols(row, i,
                                    map (fn v => (v, P_Wild)) args))
                          in
                          (* we add the row to all of the sub-matrices *)
                            app addVarRow conMap;
                            addRow(varMat, row)
                          end
(*
                      | (P_IConst n) => let
                          val (SOME mat) = findCon (conMap, LIT n)
                          in
                            mat := addRow(!mat, expandCols(row, i, []));
                            varMat
                          end
*)
                      | (P_ConApp(c, args)) => let
                          val (SOME mat) = findCon (conMap, OP c)
                          in
                            mat := addRow(!mat, expandCols(row, i, args));
                            varMat
                          end
                    (* end case *)
                  end
          val varMat = f (#rows mat, splitCol)
          in
            (List.nth(vars, i), conMap, varMat)
          end

  (******************** Translation ********************)

(* NOTE: eventually, we can merge this function with mkMatrix *)
    fun step1 rules = let
          val errState = initState()
          val rootPaths = [PATH[]]
          fun arityMismatch rator =
                raise Fail(CS.opToString rator ^ " arity mismatch in pattern")
        (* convert a pattern to a list of patterns *)
          fun doPat (path, CS.IdPat x, vmap) =
                (path, P_Wild, VMap.insert(vmap, x, path))
(*
            | doPat (sort, path, CS.P_VarAsPat(x, p), vmap) =
                doPat (sort, path, p, VMap.insert(vmap, x, path))
*)
            | doPat (path, CS.WildPat, vmap) = (path, P_Wild, vmap)
(*
            | doPat (sort, path, CS.P_IConst n, vmap) = (path, P_IConst n, vmap)
*)
            | doPat (path, CS.OpPat(rator, args), vmap) = let
                fun extendPaths (_, []) = []
                  | extendPaths (i, _::r) =
                      extendPath(path, i) :: extendPaths(i+1, r)
                val (args, vmap) = doPatList(extendPaths(0, args), args, vmap)
                in
                  (path, P_ConApp(rator, args), vmap)
                end
          and doPatList ([], [], vmap) = ([], vmap)
            | doPatList (path::r2, pat::r3, vmap) = let
                val (path, pat, vmap) = doPat (path, pat, vmap)
                val (rest, vmap) = doPatList (r2, r3, vmap)
                in
                  ((path, pat)::rest, vmap)
                end
            | doPatList _ = raise Fail "arity"  (* should never happen *)
          fun doRules [] = []
            | doRules ((pats, exp) :: rest) = let
                val (row, vmap) = doPatList (rootPaths, pats, VMap.empty)
                in
                  (row, finalState(vmap, exp)) :: doRules rest
                end
          in
            (doRules rules, errState)
          end

    fun step2 (patMatrix : 'a matrix, errState) = let
          fun genDFA (mat as {rows as (row1, q1)::rrows, cols, vars}) = (
(*DEBUG print(concat["genDFA: ", Int.toString(length rows), " rows, ", *)
(*DEBUG   Int.toString(length cols), " cols\n"]); *)
                case (chooseCol mat)
                 of NONE => (inc q1; q1)
                  | SOME i => let
                      val (splitVar, conMap, varMat) = splitAtCol(mat, i)
(*DEBUG val _ = print(concat["  split at column ", Int.toString i, "\n"]);*)
                      val lastArc = (case varMat
                             of {rows=[], ...} => let
                                  fun mkCell (_, (right, cols)) = let
                                        val cell = CELL{
                                                pat = P_Wild, down = NIL,
                                                right = right
                                              }
                                        in
                                          (cell, cell::cols)
                                        end
                                  val (row, cols) = List.foldr mkCell (NIL, []) vars
                                  val mat = {
                                          rows=[(row, errState)],
                                          cols=cols, vars=vars
                                        }
                                  in
                                    (ANY, genDFA mat)
                                  end
                              | mat => (ANY, genDFA mat)
                            (* end case *))
                      fun mkSwitchArc ({con=OP rator, args, mat}, arcs) =
                            (DECONS(rator, args), genDFA(!mat)) :: arcs
(*
                      fun mkTestArc ({con=LIT n, args=[], mat}) = (n, genDFA(!mat))
*)
                      val q = (case conMap
                             of [] => switchState(splitVar, [lastArc])
                              | ({con=OP _, ...}::_) =>
                                  switchState (
                                    splitVar,
                                    List.foldr mkSwitchArc [lastArc] conMap)
(*
                              | ({con=LIT _, ...}::_) =>
                                  testState (
                                    splitVar,
                                    List.map mkTestArc conMap,
                                    #2 lastArc)
*)
                            (* end case *))
                      in
                        inc q; q
                      end
                (* end case *))
          in
            genDFA (patMatrix)
          end

(*
  (* In step3 we minimize the DFA by combining identical states *)
    fun step3 (q0 : state) = let
          in
          end
*)

    fun compile match = let
          val match = map (fn (p, e') => ([p], e')) match
          val (rules, errState) = step1 match
          val patMatrix = mkMatrix rules
          val finalStates = map #2 (#rows patMatrix)
          val dfa = step2(patMatrix, errState)
(*
          val dfa = step3 dfa
*)
          in
(* NOTE: final states with zero reference counts are redundant and
 * a non-zero count on the error state means that the match is
 * nonexhaustive.
 *)
            dfa
          end

  (* sets and maps of paths *)
    structure K = struct
        type ord_key = path
        fun compare (PATH l1, PATH l2) = let
              fun cmp ([], []) = EQUAL
                | cmp (_, []) = GREATER
                | cmp ([], _) = LESS
                | cmp (i1::r1, i2::r2) = (case Int.compare(i1, i2)
                     of EQUAL => cmp(r1, r2)
                      | order => order
                    (* end case *))
              in
                cmp (l1, l2)
              end
      end
    structure PSet = RedBlackSetFn(K)
    structure PMap = RedBlackMapFn(K)

  end
