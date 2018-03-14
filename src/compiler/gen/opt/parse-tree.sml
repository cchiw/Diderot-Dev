(* parse-tree.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure ParseTree =
  struct

    type var = Atom.atom
    type oper = Atom.atom

    datatype specification = Spec of rule list

    and rule = Rule of pattern * pattern

    and pattern
      = WildPat
      | IdPat of var
      | OpPat of oper * pattern list

  end

