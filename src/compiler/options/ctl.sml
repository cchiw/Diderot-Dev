(* ctl.sml
 *
 * Internal compiler controls.  These may be set from the command-line, but are
 * mainly meant to be used by the developers for debugging purposes.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Ctl =
  struct

    local
      structure C = Controls
    in

    val enableLog = C.genControl {
            name = "log",
            pri = [1, 1],
            obscurity = 5,
            help = "output compiler debugging message to a log file",
            default = false
          }
    val collectStats = C.genControl {
            name = "stats",
            pri = [1, 2],
            obscurity = 5,
            help = "collect and report statistics about optimizations, etc.",
            default = false
          }
    val verbose = C.genControl {
            name = "verbose",
            pri = [1, 3],
            obscurity = 5,
            help = "print messages to stderr as each compiler stage starts and ends",
            default = false
          }

    val inline = C.genControl {
            name = "inlining",
            pri = [2, 0, 0],
            obscurity = 5,
            help = "disable inlining of user-defined functions",
            default = true
          }
    val highVN = C.genControl {
            name = "high-vn",
            pri = [2, 1, 0],
            obscurity = 5,
            help = "disable high-ir value numbering",
            default = true
          }
    val midVN = C.genControl {
            name = "mid-vn",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "disable mid-ir value numbering",
            default = true
          }
    val midContract = C.genControl {
            name = "mid-contract",
            pri = [2, 2, 1],
            obscurity = 5,
            help = "disable mid-ir contraction",
            default = true
          }
    val lowVN = C.genControl {
            name = "low-vn",
            pri = [2, 3, 0],
            obscurity = 5,
            help = "disable low-ir value numbering",
            default = true
          }
    val lowContract = C.genControl {
            name = "low-contract",
            pri = [2, 3, 1],
            obscurity = 5,
            help = "disable low-ir contraction",
            default = true
          }
    val treeContract = C.genControl {
            name = "tree-contract",
            pri = [2, 4, 0],
            obscurity = 5,
            help = "disable tree-ir contraction",
            default = true
          }
    val sliceFlag = C.genControl {
            name = "sliceFlag",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "disable slicing",
            default = true
        }
    val fullSplitFlag= C.genControl {
            name = "fullSplitFlag",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "full splitting (besides probe)",
            default = true
        }

    val  scaFlag= C.genControl {
            name = "scaFlag",
            pri = [2, 2, 0],
            obscurity = 5,
            help = " limits vector operators to scalars",
            default = false
        }
    val replaceFlag = C.genControl {
            name = "replaceFlag",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "replace fields instead of lifting",
            default = false
        }
    val readEin = C.genControl {
            name = "readEin",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "Print some variation of the intermediate representation",
            default = false
        }

    val readEin1 = C.genControl {
            name = "readEin1",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "readable version of EIN notation. One pass",
            default = false
        }

    val readEin2 = C.genControl {
            name = "readEin2",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "readable version of EIN notation. Two passes",
            default = false
        }

    val readEin3 = C.genControl {
            name = "readEin3",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "readable version of EIN notation. Three passes",
            default = false
        }

    val readEin4 = C.genControl {
            name = "readEin4",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "readable version of EIN notation. Four passes",
            default = false
        }
    val readEin5 = C.genControl {
            name = "readEin5",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "readable version of EIN notation. Five passes",
            default = false
        }


    val repMultiple = C.genControl {
            name = "repMultiple",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "readable version of EIN notation. Prints multiple versions from various rewriting passes.",
            default = false
        }

    val readEinSplit = C.genControl {
            name = "readEinSplit",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "readable version of EIN notation. Prints out the computation split into pieces. ",
            default = false
        }

    val savePDF = C.genControl {
            name = "savePDF",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "output to a txt file",
            default = false
    }

    val formatUni = C.genControl {
            name = "formatUni",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "Translate IR to unicode",
            default = false
        }

    val formatTex = C.genControl {
            name = "formatTex",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "Translate IR to latex",
            default = false
        }

    val formatWrd = C.genControl {
            name = "formatWrd",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "Translate IR to words",
            default = false
        }


    val repEin = C.genControl {
            name = "repEin",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "Translate IR to EIN",
            default = false
        }

    val  stgPoly = C.genControl {
            name = "stgPoly",
            pri = [2, 2, 0],
            obscurity = 5,
            help = "Translate after polynomial terms are expanded",
            default = false
        }

val  devF = C.genControl {
name = "dev",
pri = [2, 2, 0],
obscurity = 5,
help = "Develop (Skip simpleOpt phase)",
default = false
}


val expressionFlag = C.genControl {
name = "expressionFlag",
pri = [2, 2, 0],
obscurity = 5,
help = "terms in expression are set to tensors",
default = false
}

        val optimizeControls = [
        inline, highVN, midVN, midContract, lowVN, lowContract, treeContract,sliceFlag, fullSplitFlag, scaFlag ,replaceFlag,readEin,readEin1, readEin2, readEin3, readEin4,readEin5, repMultiple,readEinSplit,formatUni,formatTex,formatWrd,repEin, stgPoly, savePDF,devF,expressionFlag]


    val dumpPT = C.genControl {
            name = "dump-pt",
            pri = [3, 0],
            obscurity = 5,
            help = "dump the parse tree to the log file",
            default = false
          }
    val dumpAST = C.genControl {
            name = "dump-ast",
            pri = [3, 1],
            obscurity = 5,
            help = "dump the AST to the log file",
            default = false
          }
    val dumpSimple = C.genControl {
            name = "dump-simple",
            pri = [3, 2],
            obscurity = 5,
            help = "dump the SimpleAST to the log file",
            default = false
          }
    val dumpHighIR = C.genControl {
            name = "dump-high",
            pri = [3, 3],
            obscurity = 5,
            help = "dump the HighIR to the log file",
            default = false
          }
    val dumpMidIR = C.genControl {
            name = "dump-mid",
            pri = [3, 4],
            obscurity = 5,
            help = "dump the MidIR to the log file",
            default = false
          }
    val dumpLowIR = C.genControl {
            name = "dump-low",
            pri = [3, 5],
            obscurity = 5,
            help = "dump the LowIR to the log file",
            default = false
          }
    val dumpTreeIR = C.genControl {
            name = "dump-tree",
            pri = [3, 6],
            obscurity = 5,
            help = "dump the TreeIR to the log file",
            default = false
          }
    val dumpControls = [dumpPT, dumpAST, dumpSimple, dumpHighIR, dumpMidIR, dumpLowIR, dumpTreeIR]
    val compactCFG = ref true
    val dumpCFG = ref false
    val dumpAll = ref false

    val checkAST = C.genControl {
            name = "check-ast",
            pri = [4, 1],
            obscurity = 5,
            help = "check the AST after typechecking",
            default = false
          }
    val checkSimple = C.genControl {
            name = "check-simple",
            pri = [4, 2],
            obscurity = 5,
            help = "check the SimpleAST",
            default = false
          }
    val checkHighIR = C.genControl {
            name = "check-high",
            pri = [4, 3],
            obscurity = 5,
            help = "check the HighIR",
            default = false
          }
    val checkMidIR = C.genControl {
            name = "check-mid",
            pri = [4, 4],
            obscurity = 5,
            help = "check the MidIR",
            default = false
          }
    val checkLowIR = C.genControl {
            name = "check-low",
            pri = [4, 5],
            obscurity = 5,
            help = "check the LowIR",
            default = false
          }
    val checkTreeIR = C.genControl {
            name = "check-tree",
            pri = [4, 6],
            obscurity = 5,
            help = "check the TreeIR",
            default = false
          }
    val checkControls = [checkAST, checkSimple, checkHighIR, checkMidIR, checkLowIR, checkTreeIR]
    val checkAll = ref false

  (* after the controls have been set from the command line, we use this function
   * to enable logging if any of the dump or check options have been selected.
   *)
    fun resolve () = (
          if !dumpAll
            then List.app (fn ctl => C.set(ctl, true)) dumpControls
          else if !dumpCFG
            then List.app (fn ctl => C.set(ctl, true)) [dumpHighIR, dumpMidIR, dumpLowIR]
            else ();
          if !checkAll
            then List.app (fn ctl => C.set(ctl, true)) checkControls
            else ();
          if not(C.get enableLog)
          andalso List.exists C.get (dumpControls @ checkControls)
            then C.set (enableLog, true)
            else ())

    end (* local *)
    
  end
