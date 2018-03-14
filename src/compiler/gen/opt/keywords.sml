(* keywords.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Keywords : sig

    val idToken : string -> RulesTokens.token

  end = struct

    structure T = RulesTokens

  (* these are a subset of the operators; eventually, these will be either generated
   * by the IL generator tool or become part of the specification.
   *)
    val keywords = [
            "Dot",
            "Add",
            "Sub",
            "Mul",
            "Div",
            "Neg",
            "Scale",         
            "Kernel",
            "LoadImage",
            "Field",
            "SubField",
            "ScaleField",
            "NegField",
            "ProbeField",
            "Inside",
            "DiffField",
            "AddField",
            "DotField"
          ]

  (* create a keyword lookup table *)
    fun mkFind kws = let
        (* creates empty hashtable *)
          val tbl = AtomTable.mkTable (17, Fail "keywords")
          fun ins id =let
                val id=Atom.atom id
                in
                  AtomTable.insert tbl (id, T.OPER id)
                end
          val find = AtomTable.find tbl
          fun idToken id = let
                val id = Atom.atom id
                in 
                  case find id
                   of NONE => T.ID id (* does not find the kw *)
                    | SOME kw => kw (* finds the keywords*)
                  (* end case *)
                end
          in
            List.app ins kws; (* seed hash table with keywords *)
            idToken
          end

    val idToken = mkFind keywords

  end
