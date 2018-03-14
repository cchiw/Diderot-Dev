structure Diderot100Lex  = struct

    datatype yystart_state = 
COM1 | COM2 | STRING | INITIAL
    local

    structure UserDeclarations = 
      struct



    structure T = Diderot100Tokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* the depth int ref will be used for keeping track of comment depth *)
    val depth = ref 0

  (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

  (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

  (* make a string from buf *)
    fun mkString () = let
          val s = String.concat(List.rev(!buf))
          in
            buf := [];
            T.STRING s
          end

  (* make a REAL token from a substring *)
    fun mkReal ss = let
          val (isNeg, rest) = (case Substring.getc ss
                 of SOME(#"-", r) => (true, r)
                  | SOME(#"+", r) => (false, r)
                  | _ => (false, ss)
                (* end case *))
          val (whole, rest) = Substring.splitl Char.isDigit rest
          val rest = Substring.triml 1 rest (* remove "." *)
          val (frac, rest) = Substring.splitl Char.isDigit rest
          val exp = if Substring.isEmpty rest
                then 0
                else let
                  val rest = Substring.triml 1 rest (* remove "e" or "E" *)
                  in
                    #1(valOf(Int.scan StringCvt.DEC Substring.getc rest))
                  end
          in
            T.REAL(RealLit.real{
                isNeg = isNeg,
                whole = Substring.string whole,
                frac = Substring.string frac,
                exp = exp
              })
          end

  (* scan a number from a hexidecimal string *)
    val fromHexString = valOf o (StringCvt.scanString (IntInf.scan StringCvt.HEX))

  (* convert a "#version" directive to a int list *)
    fun cvtVersion s = let
        (* rest of string after "#version" prefix *)
          val rest = Substring.extract(s, 8, NONE)
        (* strip leading whitespace *)
          val vers = Substring.dropl Char.isSpace rest
        (* convert a field to an integer *)
          fun cvt ss = #1 (Option.valOf (Int.scan StringCvt.DEC Substring.getc ss))
          in
            List.map cvt (Substring.fields (fn #"." => true | _ => false) vers)
          end

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yyarg as  lexErr)(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.VERSION(cvtVersion yytext)
      end
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_orelse)
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_andalso)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_lt)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_lte)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_eqeq)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_neq)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_gte)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_gt)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_plus)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_minus)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_star)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_slash)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_mod)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_exp)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_at)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_convolve)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_Ddot)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_Ddot)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_Dotimes)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_curl)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_D)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_dot)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_dot)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_cross)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_outer)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_comp)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LP)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RP)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LB)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RB)
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LCB)
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RCB)
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;  T.COMMA)
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SEMI)
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;  T.COLON)
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;  T.HASH)
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;  T.BANG)
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_eq)
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_pluseq)
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_minuseq)
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_stareq)
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_slasheq)
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OP_modeq)
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;  T.BAR)
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DOT)
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DOTDOT)
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
       T.REAL RealLit.posInf)
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
       T.REAL RealLit.pi)
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Keywords100.idToken yytext
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.INT(valOf (IntInf.fromString yytext))
      end
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  mkReal yysubstr
      end
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;  skip ())
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN STRING; continue())
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, ["bad character `", String.toString yytext]);
                            continue()
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr(valOf(String.fromString yytext)); continue()
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; continue()
      end
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; mkString())
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL;
			    lexErr(yypos, [
                                "unexpected end-of-line encountered in string literal"
                              ]);
                            continue())
fun yyAction59 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, [
                                "bad character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue()
      end
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN COM1; skip())
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; skip())
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN COM2; skip())
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; skip())
fun yyAction65 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2023
              then yyAction21(strm, yyNO_MATCH)
            else if inp < 0wx2023
              then if inp = 0wxD8
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < 0wxD8
                  then if inp = 0wxD7
                      then yyQ56(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                      else yyAction21(strm, yyNO_MATCH)
                else if inp = 0wx2022
                  then yyQ57(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = 0wx2298
              then yyAction21(strm, yyNO_MATCH)
            else if inp < 0wx2298
              then if inp = 0wx2297
                  then yyQ58(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = 0wx22C5
              then yyQ59(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B6
              then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx3B6
              then if inp = 0wx5B
                  then yyAction49(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp = 0wx30
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                    else if inp < 0wx30
                      then if inp = 0wx27
                          then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                          else yyAction49(strm, yyNO_MATCH)
                    else if inp = 0wx3A
                      then yyAction49(strm, yyNO_MATCH)
                    else if inp < 0wx3A
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                    else if inp <= 0wx40
                      then yyAction49(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp = 0wx61
                  then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp < 0wx61
                  then if inp = 0wx5F
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                      else yyAction49(strm, yyNO_MATCH)
                else if inp = 0wx3B1
                  then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp < 0wx3B1
                  then if inp <= 0wx7A
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                      else yyAction49(strm, yyNO_MATCH)
                else if inp <= 0wx3B3
                  then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
            else if inp = 0wx3C2
              then yyAction49(strm, yyNO_MATCH)
            else if inp < 0wx3C2
              then if inp = 0wx3BD
                  then yyAction49(strm, yyNO_MATCH)
                else if inp < 0wx3BD
                  then if inp = 0wx3B9
                      then yyAction49(strm, yyNO_MATCH)
                    else if inp < 0wx3B9
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                    else if inp <= 0wx3BA
                      then yyAction49(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp = 0wx3BF
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp = 0wx3C7
              then yyAction49(strm, yyNO_MATCH)
            else if inp < 0wx3C7
              then if inp = 0wx3C5
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp <= 0wx3C9
              then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B6
              then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp < 0wx3B6
              then if inp = 0wx5B
                  then yyAction48(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp = 0wx30
                      then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                    else if inp < 0wx30
                      then if inp = 0wx27
                          then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                          else yyAction48(strm, yyNO_MATCH)
                    else if inp = 0wx3A
                      then yyAction48(strm, yyNO_MATCH)
                    else if inp < 0wx3A
                      then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                    else if inp <= 0wx40
                      then yyAction48(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp = 0wx61
                  then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp < 0wx61
                  then if inp = 0wx5F
                      then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                      else yyAction48(strm, yyNO_MATCH)
                else if inp = 0wx3B1
                  then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp < 0wx3B1
                  then if inp <= 0wx7A
                      then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                      else yyAction48(strm, yyNO_MATCH)
                else if inp <= 0wx3B3
                  then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyAction48(strm, yyNO_MATCH)
            else if inp = 0wx3C2
              then yyAction48(strm, yyNO_MATCH)
            else if inp < 0wx3C2
              then if inp = 0wx3BD
                  then yyAction48(strm, yyNO_MATCH)
                else if inp < 0wx3BD
                  then if inp = 0wx3B9
                      then yyAction48(strm, yyNO_MATCH)
                    else if inp < 0wx3B9
                      then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                    else if inp <= 0wx3BA
                      then yyAction48(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp = 0wx3BF
                  then yyAction48(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp = 0wx3C7
              then yyAction48(strm, yyNO_MATCH)
            else if inp < 0wx3C7
              then if inp = 0wx3C5
                  then yyAction48(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp <= 0wx3C9
              then yyQ60(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx7C
              then yyQ61(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B6
              then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx3B6
              then if inp = 0wx5B
                  then yyAction49(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp = 0wx30
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                    else if inp < 0wx30
                      then if inp = 0wx27
                          then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                          else yyAction49(strm, yyNO_MATCH)
                    else if inp = 0wx3A
                      then yyAction49(strm, yyNO_MATCH)
                    else if inp < 0wx3A
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                    else if inp <= 0wx40
                      then yyAction49(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp = 0wx61
                  then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp < 0wx61
                  then if inp = 0wx5F
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                      else yyAction49(strm, yyNO_MATCH)
                else if inp = 0wx3B1
                  then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp < 0wx3B1
                  then if inp <= 0wx7A
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                      else yyAction49(strm, yyNO_MATCH)
                else if inp <= 0wx3B3
                  then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
            else if inp = 0wx3C2
              then yyAction49(strm, yyNO_MATCH)
            else if inp < 0wx3C2
              then if inp = 0wx3BD
                  then yyAction49(strm, yyNO_MATCH)
                else if inp < 0wx3BD
                  then if inp = 0wx3B9
                      then yyAction49(strm, yyNO_MATCH)
                    else if inp < 0wx3B9
                      then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                    else if inp <= 0wx3BA
                      then yyAction49(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp = 0wx3BF
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp = 0wx3C7
              then yyAction49(strm, yyNO_MATCH)
            else if inp < 0wx3C7
              then if inp = 0wx3C5
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp <= 0wx3C9
              then yyQ60(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ62(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ63(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ64(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ70(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction51(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ70(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ70(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ69(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx2B
                  then yyQ69(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx30
              then yyQ70(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ68(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx45
              then if inp = 0wx30
                  then yyQ67(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ67(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ68(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ67(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ67(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyAction50(strm, yyNO_MATCH)
            else if inp < 0wx2F
              then if inp = 0wx2E
                  then yyQ65(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ66(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyAction50(strm, yyNO_MATCH)
            else if inp < 0wx2F
              then if inp = 0wx2E
                  then yyQ65(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ66(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ72(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < 0wx2F
              then if inp = 0wx2A
                  then yyQ71(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = 0wx3D
              then yyQ73(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyQ74(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ75(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ76(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ77(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx26
              then yyQ78(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ79(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ90(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ90(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ90(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ90(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx2F
              then if inp = 0wx2E
                  then yyQ89(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ88(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ87(strm', lastMatch)
            else if inp < 0wx20
              then if inp = 0wx9
                  then yyQ87(strm', lastMatch)
                else if inp < 0wx9
                  then yystuck(lastMatch)
                else if inp <= 0wxD
                  then yyQ87(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx30
              then yyQ88(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ88(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yystuck(lastMatch)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yystuck(lastMatch)
                  else yyQ87(strm', lastMatch)
            else if inp = 0wx20
              then yyQ87(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ85(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx76
              then yyQ80(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ91(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx7C
              then yyQ45(strm', lastMatch)
            else if inp < 0wx7C
              then if inp = 0wx2E
                  then yyQ31(strm', lastMatch)
                else if inp < 0wx2E
                  then if inp = 0wx25
                      then yyQ23(strm', lastMatch)
                    else if inp < 0wx25
                      then if inp = 0wx21
                          then yyQ20(strm', lastMatch)
                        else if inp < 0wx21
                          then if inp = 0wxE
                              then yyQ18(strm', lastMatch)
                            else if inp < 0wxE
                              then if inp <= 0wx8
                                  then yyQ18(strm', lastMatch)
                                  else yyQ19(strm', lastMatch)
                            else if inp = 0wx20
                              then yyQ19(strm', lastMatch)
                              else yyQ18(strm', lastMatch)
                        else if inp = 0wx23
                          then yyQ22(strm', lastMatch)
                        else if inp = 0wx22
                          then yyQ21(strm', lastMatch)
                          else yyQ18(strm', lastMatch)
                    else if inp = 0wx2A
                      then yyQ27(strm', lastMatch)
                    else if inp < 0wx2A
                      then if inp = 0wx28
                          then yyQ25(strm', lastMatch)
                        else if inp < 0wx28
                          then if inp = 0wx26
                              then yyQ24(strm', lastMatch)
                              else yyQ18(strm', lastMatch)
                          else yyQ26(strm', lastMatch)
                    else if inp = 0wx2C
                      then yyQ29(strm', lastMatch)
                    else if inp = 0wx2B
                      then yyQ28(strm', lastMatch)
                      else yyQ30(strm', lastMatch)
                else if inp = 0wx40
                  then yyQ39(strm', lastMatch)
                else if inp < 0wx40
                  then if inp = 0wx3C
                      then yyQ36(strm', lastMatch)
                    else if inp < 0wx3C
                      then if inp = 0wx3A
                          then yyQ34(strm', lastMatch)
                        else if inp < 0wx3A
                          then if inp = 0wx2F
                              then yyQ32(strm', lastMatch)
                              else yyQ33(strm', lastMatch)
                          else yyQ35(strm', lastMatch)
                    else if inp = 0wx3E
                      then yyQ38(strm', lastMatch)
                    else if inp = 0wx3D
                      then yyQ37(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx5E
                  then yyQ43(strm', lastMatch)
                else if inp < 0wx5E
                  then if inp = 0wx5C
                      then yyQ18(strm', lastMatch)
                    else if inp < 0wx5C
                      then if inp = 0wx5B
                          then yyQ41(strm', lastMatch)
                          else yyQ40(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = 0wx61
                  then yyQ40(strm', lastMatch)
                else if inp < 0wx61
                  then yyQ18(strm', lastMatch)
                else if inp = 0wx7B
                  then yyQ44(strm', lastMatch)
                  else yyQ40(strm', lastMatch)
            else if inp = 0wx3C6
              then yyQ40(strm', lastMatch)
            else if inp < 0wx3C6
              then if inp = 0wx3BB
                  then yyQ40(strm', lastMatch)
                else if inp < 0wx3BB
                  then if inp = 0wx3B1
                      then yyQ40(strm', lastMatch)
                    else if inp < 0wx3B1
                      then if inp = 0wxD7
                          then yyQ47(strm', lastMatch)
                        else if inp < 0wxD7
                          then if inp = 0wx7D
                              then yyQ46(strm', lastMatch)
                              else yyQ18(strm', lastMatch)
                          else yyQ18(strm', lastMatch)
                    else if inp = 0wx3B6
                      then yyQ40(strm', lastMatch)
                    else if inp < 0wx3B6
                      then if inp <= 0wx3B3
                          then yyQ40(strm', lastMatch)
                          else yyQ18(strm', lastMatch)
                    else if inp <= 0wx3B8
                      then yyQ40(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx3C0
                  then yyQ48(strm', lastMatch)
                else if inp < 0wx3C0
                  then if inp = 0wx3BE
                      then yyQ40(strm', lastMatch)
                    else if inp < 0wx3BE
                      then if inp = 0wx3BD
                          then yyQ18(strm', lastMatch)
                          else yyQ40(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx3C3
                  then yyQ40(strm', lastMatch)
                else if inp < 0wx3C3
                  then if inp = 0wx3C1
                      then yyQ40(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx3C5
                  then yyQ18(strm', lastMatch)
                  else yyQ40(strm', lastMatch)
            else if inp = 0wx2219
              then yyQ18(strm', lastMatch)
            else if inp < 0wx2219
              then if inp = 0wx2023
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx2023
                  then if inp = 0wx3CA
                      then yyQ18(strm', lastMatch)
                    else if inp < 0wx3CA
                      then if inp = 0wx3C7
                          then yyQ18(strm', lastMatch)
                          else yyQ40(strm', lastMatch)
                    else if inp = 0wx2022
                      then yyQ49(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx2208
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx2208
                  then if inp = 0wx2207
                      then yyQ50(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx2218
                  then yyQ51(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx2298
              then yyQ18(strm', lastMatch)
            else if inp < 0wx2298
              then if inp = 0wx221F
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx221F
                  then if inp = 0wx221E
                      then yyQ52(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx2297
                  then yyQ53(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx229C
              then yyQ18(strm', lastMatch)
            else if inp < 0wx229C
              then if inp = 0wx229B
                  then yyQ54(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx22C5
              then yyQ55(strm', lastMatch)
              else yyQ18(strm', lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ14(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ14(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ16(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ16(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx3A
                  then yyAction59(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx23
                      then yyAction59(strm, yyNO_MATCH)
                    else if inp < 0wx23
                      then if inp = 0wx22
                          then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                          else yyAction59(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction59(strm, yyNO_MATCH)
                      else yyQ15(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp = 0wx5D
                  then yyAction59(strm, yyNO_MATCH)
                else if inp < 0wx5D
                  then if inp = 0wx5C
                      then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                      else yyAction59(strm, yyNO_MATCH)
                else if inp = 0wx61
                  then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction59(strm, yyNO_MATCH)
                else if inp <= 0wx62
                  then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyAction59(strm, yyNO_MATCH)
            else if inp < 0wx73
              then if inp = 0wx6F
                  then yyAction59(strm, yyNO_MATCH)
                else if inp < 0wx6F
                  then if inp = 0wx6E
                      then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                      else yyAction59(strm, yyNO_MATCH)
                else if inp = 0wx72
                  then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx74
                  then yyQ14(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx20
                  then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < 0wx20
                  then yyAction56(strm, yyNO_MATCH)
                else if inp = 0wx22
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = 0wx5D
              then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx20
                  then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < 0wx20
                  then yyAction56(strm, yyNO_MATCH)
                else if inp = 0wx22
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = 0wx5D
              then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ12(strm', lastMatch)
            else if inp < 0wx22
              then if inp = 0wxB
                  then yyQ9(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ10(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp <= 0wx1F
                  then yyQ9(strm', lastMatch)
                  else yyQ11(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ11(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyQ13(strm', lastMatch)
                  else yyQ11(strm', lastMatch)
            else if inp <= 0wx7E
              then yyQ11(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ8(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ7(strm', lastMatch)
              else yyQ6(strm', lastMatch)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ5(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COM1 => yyQ0(!(yystrm), yyNO_MATCH)
    | COM2 => yyQ1(!(yystrm), yyNO_MATCH)
    | STRING => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(yyarg as  lexErr)(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
yyarg(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
yyarg(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
