(* mkfrags.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Program to generate a file "fragments.sml" containing a fragments structure
 * from a CATALOG file.  A CATALOG file has the following layout
 *
 *      <structure name>
 *      <input file>    <fragment name>
 *      <input file>    <fragment name>
 *      ...
 *      <input file>    <fragment name>
 *
 * The resulting file (named fragments.sml) will contain a structure with the given
 * name; the structure consists of named fragments, each of which is a string literal
 * from the specified input file.
 *)

structure MkFrags : sig

    val mkFragments : string -> unit

    val mkMakefile : string -> unit

  end = struct

    structure F = Format

  (* load the catalog from the file *)
    fun loadCatalog file = let
          val inS = TextIO.openIn file
        (* report a bogus input line *)
          fun error (lnum, ln) = raise Fail (concat[
                  "[", file, ":", Int.toString lnum, "] bogus input: \"",
                  String.toString ln, "\""
                ])
        (* get the structure name *)
          val structName = (case TextIO.inputLine inS
                 of NONE => raise Fail "empty CATALOG file"
                  | SOME ln => (case String.tokens Char.isSpace ln
                       of [name] => name
                        | _ => error (1, ln)
                      (* end case *))
                (* end case *))
          fun lp (lnum, l) = (case TextIO.inputLine inS
                 of NONE => List.rev l
                  | SOME ln => (case String.tokens Char.isSpace ln
                     of [] => lp(lnum+1, l)
                      | s1::sr => if String.isPrefix "#" s1
                          then lp(lnum+1, l)
                          else (case sr
                             of [s2] => lp (lnum+1, (s1, s2) :: l)
                              | _ => error (lnum, ln)
                            (* end case *))
                    (* end case *))
                (* end case *))
          in
            (structName, lp(2, []) before TextIO.closeIn inS)
              handle ex => (TextIO.closeIn inS; raise ex)
          end

    val smlHead = "\
          \(* %s\n\
          \ *\n\
          \ * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)\n\
          \ *\n\
          \ * COPYRIGHT (c) 2016 The University of Chicago\n\
          \ * All rights reserved.\n\
          \ *\n\
          \ * !!! THIS FILE WAS GENERATED; DO NOT EDIT !!!\n\
          \ *)\n\
          \\n\
          \structure %s =\n\
          \  struct\n\
          \"

    val smlFoot = "\
          \\n\
          \  end\n\
          \"

  (* load the contents of an ".in" file *)
    fun load srcFile = let
          val inS = TextIO.openIn srcFile
          fun lp l = (case TextIO.inputLine inS
                 of NONE => List.rev l
                  | SOME ln => lp(ln::l)
                (* end case *))
          in
            (lp [] before TextIO.closeIn inS)
              handle ex => (TextIO.closeIn inS; raise ex)
          end

    fun doFile (outS, fragDir) (srcFile, smlVar) = let
          val text = load (OS.Path.concat (fragDir, srcFile))
          fun prf (fmt, items) = TextIO.output(outS, F.format fmt items)
          in
            prf ("\n", []);
            prf ("    val %s = \"\\\n", [F.STR smlVar]);
            prf ("          \\/*---------- begin %s ----------*/\\n\\\n", [F.STR srcFile]);
            List.app (fn ln => prf("          \\%s\\\n", [F.STR(String.toString ln)])) text;
            prf ("          \\/*---------- end %s ----------*/\\n\\\n", [F.STR srcFile]);
            prf ("          \\\"\n", [])
          end

    fun mkFragments dir = let
          val fragDir = OS.Path.concat(dir, "fragments")
          val catalogFile = OS.Path.concat(fragDir, "CATALOG")
          val fragFile = OS.Path.concat(dir, "fragments.sml")
          val (structName, catalog) = if OS.FileSys.access(catalogFile, [OS.FileSys.A_READ])
                then loadCatalog catalogFile
                else raise Fail(concat["cannot find \"", catalogFile, "\""])
          val outS = TextIO.openOut fragFile
          fun prf (fmt, items) = TextIO.output(outS, F.format fmt items)
          in
            prf (smlHead, [F.STR(OS.Path.file fragFile), F.STR structName]);
            List.app (doFile (outS, fragDir)) catalog;
            prf (smlFoot, []);
            TextIO.closeOut outS
          end

    val mkHead = "\
          \# fragments.gmk\n\
          \#\n\
          \# This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)\n\
          \#\n\
          \# COPYRIGHT (c) 2016 The University of Chicago\n\
          \# All rights reserved.\n\
          \#\n\
          \# !!! THIS FILE WAS GENERATED; DO NOT EDIT !!!\n\
          \#\n\
          \\n\
          \"

    fun mkMakefile dir = let
          val fragDir = OS.Path.concat(dir, "fragments")
          val catalogFile = OS.Path.concat(fragDir, "CATALOG")
          val makefile = OS.Path.concat(dir, "fragments.gmk")
          val (_, catalog) = if OS.FileSys.access(catalogFile, [OS.FileSys.A_READ])
                then loadCatalog catalogFile
                else raise Fail(concat["cannot find \"", catalogFile, "\""])
          val outS = TextIO.openOut makefile
          fun prf (fmt, items) = TextIO.output(outS, F.format fmt items)
          fun prDep file = prf(" \\\n    %s/fragments/%s", [F.STR dir, F.STR file])
          in
            prf (mkHead, []);
            prf ("%s/fragments.sml:", [F.STR dir]);
            prDep "CATALOG";
            List.app (fn (srcFile, _) => prDep srcFile) catalog;
            prf ("\n", []);
            TextIO.closeOut outS
          end

  end
