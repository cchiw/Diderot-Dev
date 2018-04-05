
structure ScanEin : sig

    val readIR_Single: (MidIR.var * MidIR.rhs)*string -> int
    val readIR_Split : (MidIR.var * MidIR.rhs) list -> int
    val expand : MidIR.var * HighIR.rhs list -> int


  end = struct
   
    structure E = Ein
    structure SrcIR = HighIR
    structure DstIR = MidIR
    structure ScanR= ScanRewrite
    structure R = ReadTy
    val cvt_single =  ScanR.cvt_single

    fun printToOutStream outstream str = let val os = outstream
        in
        TextIO.output(os,str);
        TextIO.closeOut os
        end;

    (* ************************************************************* *)
    fun readIR_Base (lname,anames,e,level_init, filename, readIR_PDF) = let
        (*what format do we use*)
        val readIR_Uni = Controls.get Ctl.formatUni
        val readIR_Latex = Controls.get Ctl.formatTex
        val readIR_Word = Controls.get Ctl.formatWrd
        val repMultiple = Controls.get Ctl.repMultiple
        val repEin  = Controls.get Ctl.repEin
        (*level*)
        val repMultiple = Controls.get Ctl.repMultiple
        val readIR_1 = Controls.get Ctl.readEin1
        val readIR_2 = Controls.get Ctl.readEin2
        val readIR_3 = Controls.get Ctl.readEin3
        val readIR_4 = Controls.get Ctl.readEin4
        val readIR_5 = Controls.get Ctl.readEin5
        val level = level_init
        val level = if(repMultiple) then 0 else  level
        val level = if(readIR_1)  then 1 else  level
        val level = if(readIR_2)  then 2 else  level
        val level = if(readIR_3)  then 3 else  level
        val level = if(readIR_4)  then 4 else  level
        val level = if(readIR_5)  then 5 else  level

        (* single or multiple passes *)
        fun scan_single_formatted  (level,op_n ,tf) =
            let

                val (_, nsurface) = cvt_single  (level, op_n,tf,lname, e, anames)
            in
                concat[lname, nsurface, "\n"]
            end
        fun scan_multiplePasses (_,op_n,tf) = let
            (*does multiple rewriting passes*)
            fun iter (5,exp, es) = (exp, List.rev es) (*stop at last step*)
              | iter (level,exp,es) = let
                    val (exp, nsurface) = cvt_single  (level, op_n,tf,lname, e, anames)
                    in iter(level+1,exp, nsurface::es) end

            val (exp, es) = iter(1,e, [])
            in (concat[lname, concat(es),"\n"]) end
        (*make unicode version*)
        fun mkTxt(txt0,ending) = (printToOutStream (TextIO.openOut (concat[filename,ending])) txt0;1)
        (*from string to latex file*)
        fun mkLat(txt0) =  mkTxt(concat["\n\\documentclass{article}\n\\begin{document}",txt0,"\\end{document}"],".latex")

        fun output (es, tf) =
            (case (readIR_PDF, tf)
                of (true, true) =>  mkLat(es)
                | (true, _)     =>  mkTxt(es,".txt")
                | _ =>  (print(es);1)
            (* end case*))

        fun scan_f   (op_n, tf)  = let
            val es = (case level
                of 0     => scan_multiplePasses(level,op_n, tf)
                | ~1   => ""
                | _ => scan_single_formatted(level,op_n, tf)
                (* end case*))
            in output (es, tf) end

        in  (case (repEin, readIR_Uni,readIR_Latex)
            of (true, true, _) =>
                let
                val name1 = EinPP.toString_reader(e)
                val m = concat["lhsvar:", lname,"\n\t",  name1, "\n\tArgs:(",String.concatWith"," anames,")"]
                val _ = output(m, false)
                in 1 end
            | (true, _,true)  =>  let
                val e = EinSums.transform (ScanRewrite.rewriteE (e,true))
                val name1 = EinPPLat.toString_reader(e)
                val m = concat["lhsvar:","$",lname,"=$\\\\ \\phantom{++}",  name1, "\\\\ \\phantom{++}Args:(",String.concatWith"," anames,")"]
                val _ = output(m, true)
                in 1 end
            | (true, _, _) =>
                let
                val name1 = EinPP.toString_reader(e)
                val m = concat["lhsvar:", lname,"\n\t",  name1, "\n\tArgs:(",String.concatWith"," anames,")"]
                val _ = output(m, false)
                val name1 = EinPPLat.toString_reader(e)
                val m = concat["lhsvar:","$",lname,"=$\\\\ \\phantom{++}",  name1, "\\\\ \\phantom{++}Args:(",String.concatWith"," anames,")"]
                val _ = output(m, true)
                in 1 end
            | (_, true, false) => scan_f(R.op_u, false) (*unicode*)
            | (_, false, true) => scan_f(R.op_l, true)  (*latex*)
            |  _ => (scan_f(R.op_u, false);scan_f(R.op_l,true))
            (* end case*))
        end


    (* ************************************************************* *)

    (*command lie arguments are used to decide rewrite and format*)
    fun readIR_Single (newbie, filename) = let
        (*reading ein controls*)
        val readIR_PDF = Controls.get Ctl.savePDF
        (*specific*)
        val level_init =2 (*level*)
        val (lhs,DstIR.EINAPP(ein,a)) = newbie
        val lname = DstIR.Var.name  lhs
        val anames = List.map (fn v=> DstIR.Var.name(v)) a
        in
            readIR_Base (lname, anames, ein, level_init,filename,readIR_PDF)
        end

    (*call with direct-style operator printIR() *)
    fun expand (lhs, SrcIR.EINAPP(ein, args)::es) = let
        (* transform *)
        val ein = EinSums.transform ein
        (*specific *)
        val lname = DstIR.Var.name  lhs
        val anames = List.map (fn v=> SrcIR.Var.name(v)) args
        val level_init =2

        (*by argument*)
        val (readIR_PDF ,filename) = (case es
            of [SrcIR.LIT(Literal.String filename)] => (true,filename)
            | []                                    => (Controls.get Ctl.savePDF,"output_tmp")
            (* end case*))
        in
            readIR_Base (lname, anames, ein, level_init,filename,readIR_PDF)
        end

    (* ************************************************************* *)
    fun readIR_Split (newbies) =
            let
            (*reading ein controls*)
            val readIR_Split = Controls.get Ctl.readEinSplit
            val readIR_PDF = Controls.get Ctl.savePDF
            val repMultiple =  false
            (*reading ein format s*)
            val readIR_Uni = Controls.get Ctl.formatUni
            val readIR_Latex = Controls.get Ctl.formatTex
            val readIR_Word = Controls.get Ctl.formatWrd
            val repEin  = Controls.get Ctl.repEin

            (*use latex by default*)
            val op_n = if readIR_Uni then R.op_u  else if readIR_Word then  R.op_w else R.op_l

            fun scan_pieces (lhs,DstIR.EINAPP(e,a)) = let
                val lname = DstIR.Var.name  lhs
                val anames = List.map (fn v=> DstIR.Var.name(v)) a
                val level = 3
                val (exp, nsurface) = ScanR.cvt_single  (level, op_n,readIR_Latex,lname, e, anames)
                val ss = (concat[lname, nsurface, "\n"])

                in ss end
            | scan_pieces  _ =  ""
        in
            if(readIR_Split)
            then (List.map (fn e1 => print(scan_pieces(e1))) newbies;1)
            else 1
        end

  end (* HandleEin *)
