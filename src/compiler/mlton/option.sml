(* option.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

signature OPTION_EXT =
  sig

    include OPTION

    val isNone : 'a option -> bool

  end

structure OptionExt : OPTION_EXT =
  struct

    open Option

    fun isNone NONE = true
      | isNone (SOME _) = false

  end
