(* generator-sig.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * This code was ported from the Manticore project (http://manticore.cs.uchicago.edu)
 *)

signature GENERATOR =
  sig

  (* name of template file *)
    val template : string

  (* destination path relative to root of Manticore source tree *)
    val path : string

    val hooks : TextIO.outstream * LoadFile.log_file_desc -> (string * (unit -> unit)) list

  end
