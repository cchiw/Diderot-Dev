(* main.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Main =
  struct

    structure PP = TextIOPP

  (* prettyprinter for rule RHS *)
    fun ppRHS (ppStrm, pat) = (case pat
           of CheckSpec.WildPat => PP.string ppStrm "_"
            | CheckSpec.IdPat x => PP.string ppStrm (CheckSpec.varToString x)
            | CheckSpec.OpPat(rator, args) => (
                PP.string ppStrm (CheckSpec.opToString rator);
                PP.string ppStrm "(";
                case args
                 of [] => ()
                  | [p] => ppRHS (ppStrm, p)
                  | (p::r) => (
                      ppRHS (ppStrm, p);
                      List.app
                        (fn p => (PP.string ppStrm ","; ppRHS (ppStrm, p))) r)
                (* end case *);
                PP.string ppStrm ")")
          (* end case *))

    val ppDFA = DumpDFA.ppDFA ppRHS

    fun quitWithError srcFile = raise Fail("Error in compiling " ^ srcFile)

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm = (
          Error.report (TextIO.stdErr, errStrm);
          if Error.anyErrors errStrm
            then quitWithError (Error.sourceFile errStrm)
            else ())

    fun doFile file = let
          val inStrm = TextIO.openIn file
          val errStrm = Error.mkErrStream file
          val pt = RulesParser.parse (errStrm, inStrm)
          val _ = TextIO.closeIn inStrm
          in
            case pt
             of SOME spec => let
                  val CheckSpec.Spec rules = CheckSpec.check(errStrm, spec)
                  val dfa = CompileMatch.compile
                        (List.map (fn (CheckSpec.Rule r) => r) rules)
                  in
                    ppDFA (PP.openOut{dst=TextIO.stdOut, wid=120}, dfa)
                  end
              | NONE => checkForErrors errStrm
            (* end case *)
          end

  end

          