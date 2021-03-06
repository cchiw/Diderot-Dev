(* load-file.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * This code was ported from the Manticore project (http://manticore.cs.uchicago.edu)
 *)

structure LoadFile : sig

    datatype event = EVT of {
	name : string,			(* event name *)
	id : int,			(* unique integer ID *)
	args : EventSig.arg_desc list,	(* arguments *)
	sign : string,			(* signature *)
        isSrc : bool,			(* this event is the source of a dependent event
					 * and has a new-id argument.
					 *)
	desc : string			(* description *)
      }

    type log_file_desc = {
	date : string,
	version : {major : int, minor : int, patch : int},
	fat : bool,
	events : event list
      }

    val loadFile : string -> log_file_desc

    val isSource : event -> bool

  (* helper functions *)
    val filterEvents : (event -> bool) -> log_file_desc -> log_file_desc
    val applyToEvents : (event -> unit) -> log_file_desc -> unit
    val foldEvents : ((event * 'a) -> 'a) -> 'a -> log_file_desc -> 'a

  end = struct

    structure J = JSON

    datatype event = EVT of {
	name : string,
	id : int,
	args : EventSig.arg_desc list,
	sign : string,
        isSrc : bool,			(* this event is the source of a dependent event
					 * and has a new-id argument.
					 *)
	desc : string
      }

    type log_file_desc = {
	date : string,
	version : {major : int, minor : int, patch : int},
	fat : bool,
	events : event list
      }

    fun isSource (EVT{isSrc, ...}) = isSrc

    fun findField (J.OBJECT fields) = let
	  fun find lab = (case List.find (fn (l, v) => (l = lab)) fields
		 of NONE => NONE
		  | SOME(_, v) => SOME v
		(* end case *))
	  in
	    find
	  end
      | findField _ = raise Fail "expected object"

    fun lookupField findFn lab = (case findFn lab
	   of NONE => raise Fail(concat["no definition for field \"", lab, "\""])
	    | SOME v => v
	  (* end case *))

    fun cvtArray cvtFn (J.ARRAY vl) = List.map cvtFn vl
      | cvtArray cvtFn _ = raise Fail "expected array"

  (* fold a function over a JSON array value *)
    fun foldl cvtFn init (J.ARRAY vl) = List.foldl cvtFn init vl
      | foldl _ _ _ = raise Fail "expected array"

    fun findInt find = let
	  fun get lab = (case find lab of J.INT r => r | _ => raise Fail "expected integer")
	  in
	    get
	  end

    fun findBool find = let
	  fun get lab = (case find lab of J.BOOL b => b | _ => raise Fail "expected boolean")
	  in
	    get
	  end

    fun findString find = let
	  fun get lab = (case find lab
		 of J.STRING s => s
		  | _ => raise Fail "expected string"
		(* end case *))
	  in
	    get
	  end

    fun cvtArg (obj, (loc, ads)) = let
	  val find = findField obj
	  val lookup = lookupField find
	  val name = findString lookup "name"
	  val ty = (case EventSig.tyFromString (findString lookup "ty")
		 of SOME ty => ty
		  | NONE => raise Fail "bogus type"
		(* end case *))
	  val (loc, nextLoc) = (case find "loc"
		 of SOME(J.INT n) => let
		      val loc = Word.fromLargeInt n
		      in
		      (* NOTE: we don't check that loc is properly aligned here;
		       * that is checked later when we compute the signature.
		       *)
			(loc, loc + #sz(EventSig.alignAndSize ty))
		      end
		  | SOME _ => raise Fail "expected integer for \"loc\" field"
		  | NONE => let
		      val {align, sz, ...} = EventSig.alignAndSize ty
		      val loc = EventSig.alignLoc (loc, align)
		      in
			(loc, loc+sz)
		      end
		(* end case *))
	  val desc = findString lookup "desc"
	  val ad = {
		  name = name,
		  ty = ty,
		  loc = loc,
		  desc = desc
		}
	  in
	    (nextLoc, ad::ads)
	  end

    fun cvt obj = let
	  val find = lookupField(findField obj)
	  val version = (case find "version"
		 of J.ARRAY[J.INT v1, J.INT v2, J.INT v3] => {
			major = Int.fromLarge v1,
			minor = Int.fromLarge v2,
			patch = Int.fromLarge v3
		      }
		  | _ => raise Fail "bogus version"
		(* end case *))
	  val nextId = ref 1	(* ID 0 is unused *)
	  fun cvtEvent obj = let
		val find = lookupField (findField obj)
		val name = findString find "name"
		val args = let
		      val (_, args) = foldl cvtArg (EventSig.argStart, []) (find "args")
		      in
			List.rev args
		      end
		val id = !nextId
		val isSrc = findBool find "is-src"
		in
		  nextId := id + 1;
		  EVT{
		      name = name, id = id, args = args, isSrc = isSrc,
		      sign = EventSig.signOf args, desc = findString find "desc"
		    }
		end
	  in {
	    date = findString find "date",
	    version = version,
	    fat = false,
	    events = cvtArray cvtEvent (find "events")
	  } end

    fun loadFile file = cvt (JSONParser.parseFile file)

  (* helper functions *)
    fun filterEvents pred {date, version, fat, events} = {
	    date=date, version=version, fat = fat,
	    events = List.filter pred events
	  }
    fun applyToEvents f {date, version, fat, events} = List.app f events
    fun foldEvents f init {date, version, fat, events} = List.foldl f init events

  end
