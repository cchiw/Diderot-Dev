(* run-cc.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Support for running the C++ compiler to compile and link the generated code.
 *)

structure RunCC : sig

  (* compile ("base", cflags) -- compiles the file "base.cxx" to produce "base.o". *)
    val compile : string * string list -> unit

  (* linkExec ("base", opts) -- links base.o to create an executable using the
   * given options (libraries, etc.)
   *)
    val linkExec : string * string list -> unit

  (* linkLib ("base", opts) -- links base.o to create using the given options (libraries, etc.) *)
    val linkLib : string * string list -> unit

  end = struct

    fun system cmd = (
          Log.msg [cmd, "\n"];
          if OS.Process.isSuccess(OS.Process.system cmd)
            then ()
            else raise Fail "error compiling/linking")

    fun compile (baseName, cxxflags) = let
          val cxxFile = OS.Path.joinBaseExt{base=baseName, ext=SOME "cxx"}
	  val cmdArgs = [cxxFile]
	  val cmdArgs = if OS.Path.dir baseName <> ""
	      (* the C++ compiler will, by default, put the ".o" file in the current working
	       * directory, so we need to specify the path for the .o file.
	       *)
		then "-o" :: OS.Path.joinBaseExt{base=baseName, ext=SOME "o"} :: cmdArgs
		else cmdArgs
	  val cmdArgs = Paths.ccxx :: "-c" :: cxxflags @ cmdArgs
          val cmd = String.concatWith " " cmdArgs
          in
            PhaseTimer.withTimer Timers.timeCC system cmd
          end

    fun linkExec (baseName, ldOpts) = let
          val objFile = OS.Path.joinBaseExt{base=baseName, ext=SOME"o"}
          val exeFile = baseName
          val cmd = String.concatWith " " ([Paths.ccxx, "-o", exeFile, objFile] @ ldOpts)
          in
            PhaseTimer.withTimer Timers.timeCC system cmd
          end

    fun linkLib (baseName, ldOpts) = let
          val objFile = OS.Path.joinBaseExt{base=baseName, ext=SOME"o"}
          val tmpFile = let
              (* on Linux systems, the rename fails if the src and dst are on
               * different devices, so we create the temp file in the same
               * directory as the final target.
               *)
                val {file, ...} = OS.Path.splitDirFile(OS.FileSys.tmpName())
                val {dir, ...} = OS.Path.splitDirFile baseName
                in
                  OS.Path.joinDirFile{
                      dir = dir,
                      file = OS.Path.joinBaseExt{
                          base = file,
                          ext = SOME "o"
                        }
                    }
                end
          val cmd = String.concatWith " " ([Paths.ld, "-r", "-o", tmpFile, objFile] @ ldOpts)
          fun link () = (
                system cmd;
                Log.msg ["rename ", tmpFile, " to ", objFile, "\n"];
                OS.FileSys.rename{old=tmpFile, new=objFile}
                  handle ex => (OS.FileSys.remove tmpFile; raise ex))
          in
            PhaseTimer.withTimer Timers.timeCC link ()
          end

  end
