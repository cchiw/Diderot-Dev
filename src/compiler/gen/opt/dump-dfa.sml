(* dump-dfa.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Routines to print out the internal data structures in the pattern-match
 * compiler.
 *)

structure DumpDFA =
  struct

    local
      structure CM = CompileMatch
      structure CS = CheckSpec
      structure Q = Queue
      structure F = Format
      fun path2str (CM.PATH l) =
            concat ("root"
              :: (List.foldr (fn(i, l) => "$"::Int.toString i::l) [] l))
      structure PP = TextIOPP

      datatype 'a stateQ = SQ of {q : 'a CM.state Q.queue, marked : word list ref}
      fun mkQueue s0 = let
            val q = Q.mkQueue()
            in
              Q.enqueue(q, s0);
              SQ{q = q, marked = ref []}
            end
      fun insert (SQ{q, ...}, s) = Q.enqueue(q, s)
      fun getState (sq as SQ{q, marked}) =
            if Q.isEmpty q
              then NONE
              else let
                val s = Q.dequeue q
                val id = CM.stamp s
                in
                  if (List.exists (fn id' => (id = id')) (!marked))
                    then getState sq
                    else (
                      marked := id :: !marked;
                      SOME s)
                end
    in

    fun ppDFA ppRHS (ppStrm, q0 : 'a CM.state) = let
          val stateQ = mkQueue q0
          val str = PP.string ppStrm
          val tok = PP.token ppStrm
          val sp = PP.space ppStrm
          fun ppStateId s = str("state"^Word.toString(CM.stamp s))
          fun ppArgs [] = ()
            | ppArgs [p] = str (path2str p)
            | ppArgs (p::r) = (str (path2str p); str ","; sp 1; ppArgs r)
          fun ppArc (pat, q) = (
                PP.newline ppStrm;
                PP.openHBox ppStrm;
                  case pat
                   of CM.ANY => str "_"
                    | CM.DECONS(con, []) => str(CS.opToString con)
                    | CM.DECONS(con, args) => (
                        str(CS.opToString con); str "(";
                        ppArgs args;
                        str ")")
                  (* end case *);
                  sp 1; str "=>"; sp 1;
                  ppNextState q;
                PP.closeBox ppStrm)
          and ppNextState q =
                if ((CM.refCnt q) > 1)
                  then (str "goto"; sp 1; ppStateId q; insert(stateQ, q))
                  else ppState(false, q)
          and ppState (doLabel, q) = (
                PP.openHBox ppStrm;
                  if doLabel
                    then (
                      ppStateId q; str "["; str(Int.toString(CM.refCnt q)); str "]";
                      str ":"; sp 1)
                    else ();
                  case (CM.kind q)
                   of CM.SWITCH(v, arcs) => (
                        str "switch"; sp 1; str(path2str v);
                        PP.openVBox ppStrm (PP.Abs 2);
                          app ppArc arcs;
                        PP.closeBox ppStrm)
                    | CM.FINAL(vMap, e) => let
                        fun pVar (x, path) = (
                              str(path2str path); str "/"; str(CS.varToString x))
                        fun pVar' vp = (str ","; sp 1; pVar vp)
                        in
                          str "execute"; sp 1;
                          PP.openHBox ppStrm;
                            ppRHS(ppStrm, e);
                            case CheckSpec.VMap.listItemsi vMap
                             of [] => ()
                              | (item::r) => (
                                  sp 1;
                                  str "[";
                                  pVar item;
                                  List.app pVar' r;
                                  str "]")
                            (* end case *);
                          PP.closeBox ppStrm
                        end
                    | CM.ERROR => str "error"
                  (* end case *);
                PP.closeBox ppStrm)
          fun ppStates () = (case getState stateQ
                 of NONE => ()
                  | (SOME s) => (ppState(true, s); PP.newline ppStrm; ppStates())
                (* end case *))
          in
            PP.openVBox ppStrm (PP.Abs 0);
              ppStates ();
            PP.closeBox ppStrm
          end

    end (* local *)

  end;

