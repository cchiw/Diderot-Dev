(* check-spec.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * This is a stub file that will eventually be replaced by a typechecker for the
 * rewrite rules.
 *)

structure CheckSpec =
  struct

    type var = Atom.atom
    type oper = Atom.atom

    datatype specification = datatype ParseTree.specification

    datatype rule = datatype ParseTree.rule

    datatype pattern = datatype ParseTree.pattern

    fun check (errStrm, spec) = spec

    val varToString = Atom.toString

    val sameOp = Atom.same
    val compareOp = Atom.compare
    val opToString = Atom.toString

  (* finite maps with variable keys *)
    structure VMap = AtomMap

  end
