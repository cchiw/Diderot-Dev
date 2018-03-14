(* main.sml
 *
 * MLton wrapper for the MkFrags generator code.  We use the command name
 * to select the operation.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

fun fail () = OS.Process.exit OS.Process.failure;

fun main (cmdName, [dir]) = (case OS.Path.file cmdName
       of "mkmk" => MkFrags.mkMakefile dir
        | "mkfrags" => MkFrags.mkFragments dir
        | _ => fail()
      (* end case *))
  | main _ = fail();

val _ = main (CommandLine.name(), CommandLine.arguments()) handle _ => fail();
