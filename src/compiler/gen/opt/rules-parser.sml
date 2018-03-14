(* rules-parser.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure RulesParser : sig

    val parse : (Error.err_stream * TextIO.instream) -> ParseTree.specification option

  end = struct

  (* Put together lexer and parser *)
    structure DP = RulesParseFn(RulesLex)

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (* map tokens to strings *)
    fun tokToString (RulesTokens.ID x) = Atom.toString x
      | tokToString (RulesTokens.OPER x) = Atom.toString x
      | tokToString tok = RulesTokens.toString tok

  (* error function for parsers *)
    val parseErr = Error.parseError tokToString

  (* parse a file, returning a parse tree *)
    fun parse (errStrm, inStrm) = let
          fun get () = TextIO.input inStrm
          val lexer = RulesLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
          in
            case DP.parse lexer (RulesLex.streamify get)
             of (SOME pt, _, []) => (TextIO.closeIn inStrm; SOME pt)
              | (_, _, errs) => (
                  TextIO.closeIn inStrm;
                  List.app (parseErr errStrm) errs;
                  NONE)
            (* end case *)
          end

  end
