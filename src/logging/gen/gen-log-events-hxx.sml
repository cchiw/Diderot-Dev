(* gen-log-events-hxx.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * This code was ported from the Manticore project (http://manticore.cs.uchicago.edu)
 *)

structure GenLogEventsHxx : GENERATOR =
  struct

    val template = "log-events_hxx.in"
    val path = "src/lib/include/diderot/log-events.hxx"

    fun hooks (outS, logDesc as {date, version, fat, events} : LoadFile.log_file_desc) = let
	  fun prl l = TextIO.output(outS, concat l)
	  fun genVersion () = (
		prl ["#define DIDEROT_LOG_VERSION_MAJOR ", Int.toString (#major version), "\n"];
		prl ["#define DIDEROT_LOG_VERSION_MINOR ", Int.toString (#minor version), "\n"];
		prl ["#define DIDEROT_LOG_VERSION_PATCH ", Int.toString (#patch version), "\n"];
		if fat then prl ["#define DIDEROT_FAT_LOG_EVENTS\n"] else ())
	  fun prDef (name, id, desc) = prl [
		  "            ", name, " = ", Int.toString id, ", /* ", desc, " */\n"
		]
	  fun genDef (LoadFile.EVT{id = 0, name, desc, ...}) = prDef (name, 0, desc)
	    | genDef (LoadFile.EVT{id, name, desc, ...}) = prDef (name^"Evt", id, desc)
	  in [
	    ("DATE", fn () => prl ["#define DIDEROT_LOG_VERSION_DATE ", date, "\n"]),
	    ("VERSION", genVersion),
	    ("LOG-EVENTS", fn () => LoadFile.applyToEvents genDef logDesc)
	  ] end

  end
