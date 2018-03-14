(* log.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Log : sig

    val enabled : unit -> bool

    val init : string -> unit

    val logFile : unit -> TextIO.outstream

    val msg : string list -> unit

    val msg' : (unit -> string list) -> unit

  (* conditional dumping and checking of IR *)
    val after : {
            dumpCtl : bool Controls.control,
            checkCtl : bool Controls.control,
            output : TextIO.outstream * string * 'prog -> unit,
            checkIR : string * 'prog -> bool
          } -> string * 'prog -> 'prog

    val reportTiming : PhaseTimer.timer -> unit

  end = struct

    val enabledFlg = ref false
    val logStrm : TextIO.outstream option ref = ref NONE

    fun enabled () = !enabledFlg

    fun init file = (case !logStrm
           of NONE => let
                val outS = TextIO.openOut file
                in
                  enabledFlg := true;
                (* turn off buffering *)
                  TextIO.StreamIO.setBufferMode (TextIO.getOutstream outS, IO.NO_BUF);
                  logStrm := SOME outS
                end
            | SOME strm => raise Fail "multiple initialization of log file"
          (* end case *))

    fun logFile () = (case !logStrm
           of NONE => (init "/dev/null"; enabledFlg := false; logFile())
            | SOME outS => outS
          (* end case *))

    fun msg s = if !enabledFlg then TextIO.output(logFile(), String.concat s) else ()

    fun msg' msgFn = if !enabledFlg then TextIO.output(logFile(), String.concat(msgFn())) else ()

    fun after {dumpCtl, checkCtl, output, checkIR} (phase, prog) = let
          fun dump () = output(logFile(), "after "^phase, prog)
          in
            if Controls.get dumpCtl
              then dump()
              else ();
            if Controls.get checkCtl andalso checkIR("after " ^ phase, prog)
              then (
                if not(Controls.get dumpCtl)  (* avoid duplication *)
                  then dump()
                  else ();
                TextIO.output(TextIO.stdErr, concat [
                    "***** Internal error after ", phase, ": see log file for details\n"
                  ]);
                OS.Process.exit OS.Process.failure)
              else prog
          end

    fun reportTiming timer = PhaseTimer.report (logFile(), timer)

  end

