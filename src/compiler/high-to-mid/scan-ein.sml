
structure ScanEin : sig

    val readEinSingle: MidIR.var * MidIR.rhs -> int
    val readEinSplit : MidIR.var * MidIR.rhs -> int
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

    fun readEinSingleT (lname,anames,e,level, filename, readEinPDF, readEinUni,readEinLatex, readEinRewrite) = let
        (* ************************************************************* *)
        fun scan_single_formatted  (level,op_n ,tf) =
            let
                val (_, nsurface) = cvt_single  (level, op_n,tf,lname, e, anames)
            in concat[lname, nsurface, "\n"]
            end

        (*does multiple rewriting passes*)
        fun scan_multiplePasses (op_n,tf) = let
            fun iter (5,exp, es) = (exp, List.rev es) (*stop at last step*)
              | iter (level,exp,es) = let
                    val (exp, nsurface) = cvt_single  (level, op_n,tf,lname, e, anames)
                    in iter(level+1,exp, nsurface::es) end

            val (exp, es) = iter(1,e, [])
            in (concat[lname, concat(es),"\n"]) end
        (* ************************************************************* *)
        (*make unicode version*)
        fun mkTxt(txt0,ending) =
            (printToOutStream (TextIO.openOut (concat[filename,ending])) txt0;1)
        (*from string to latex file*)
        fun mkLat(txt0) = let
                val txt1 = concat["\n\\documentclass{article}\n\\begin{document}",txt0,"\\end{document}"]
            in  mkTxt(txt1,".latex")
            end

        (* ************************************************************* *)

        fun scan_single_uni   ()  =
            if(readEinRewrite) then scan_multiplePasses (R.op_u, false)
            else  scan_single_formatted (level, R.op_u, false)
        fun scan_single_latex ()  =
            if(readEinRewrite) then scan_multiplePasses (R.op_l, true)
            else scan_single_formatted (level, R.op_l, true)
        fun scan_single_uniR   ()   = mkTxt(scan_single_uni(),".txt")
        fun scan_single_latexR   () = mkLat(scan_single_latex())
        in
            if(readEinPDF)
                then if(readEinUni)    then scan_single_uniR()
                else if(readEinLatex)  then scan_single_latexR()
                else (scan_single_uniR() ;scan_single_latexR())
            else if(readEinUni) then (print(scan_single_uni());1)
            else if(readEinLatex) then (print(scan_single_latex());1)
            else  (print(scan_single_uni());print(scan_single_latex());1)
        end
        (* ************************************************************* *)

    (*command lie arguments are used to decide rewrite and format*)
    fun readEinSingle (newbie) = let
        (*reading ein controls*)
        val readEinRewrite = Controls.get Ctl.readEinRewrite
        val readEin1 = Controls.get Ctl.readEin1
        val readEin2 = Controls.get Ctl.readEin2
        val readEin3 = Controls.get Ctl.readEin3
        val readEin4 = Controls.get Ctl.readEin4
        val readEin5 = Controls.get Ctl.readEin5

        (*what format do we use*)
        val readEinUni = Controls.get Ctl.readEinUni
        val readEinLatex = Controls.get Ctl.readEinLatex        (*use latex by default*)
        val readEinWord = Controls.get Ctl.readEinWord
        val readEinPDF = Controls.get Ctl.readEinPDF

        val level = ~1
        val level = if(readEinRewrite) then 0 else  level
        val level = if(readEin1)  then 1 else  level
        val level = if(readEin2)  then 2 else  level
        val level = if(readEin3)  then 3 else  level
        val level = if(readEin4)  then 4 else  level
        val level = if(readEin5)  then 5 else  level
        val filename = "output_tmp"

        val (lhs,DstIR.EINAPP(e,a)) = newbie
        val lname = DstIR.Var.name  lhs
        val anames = List.map (fn v=> DstIR.Var.name(v)) a

        val _  =  if(level>=0) then
           readEinSingleT (lname,anames,e,level, filename, readEinPDF, readEinUni,readEinLatex, readEinRewrite)
            else 1
        in 1
        end

    (* ************************************************************* *)
    fun readEinSplit (newbie) = let
        (*reading ein controls*)
        val readEinSplit = Controls.get Ctl.readEinSplit
        (*what format do we use*)
        val readEinUni = Controls.get Ctl.readEinUni
        val readEinLatex = Controls.get Ctl.readEinLatex
        val readEinWord = Controls.get Ctl.readEinWord
        (*use latex by default*)
        val op_n = if readEinUni then R.op_u  else if readEinWord then  R.op_w else R.op_l

        fun scan_pieces (lhs,DstIR.EINAPP(e,a)) = let
            val lname = DstIR.Var.name  lhs
            val anames = List.map (fn v=> DstIR.Var.name(v)) a
            val level = 3
            val (exp, nsurface) = ScanR.cvt_single  (level, op_n,readEinLatex,lname, e, anames)
            val _ = print (concat[lname, nsurface, "\n"])
            in 1 end
        | scan_pieces  _ =  1


        val _ = if(readEinSplit)  then  scan_pieces (newbie) else 1
        in
            1
        end

    fun expand (lhs, [SrcIR.LIT(Literal.String s), SrcIR.EINAPP(ein, args)]) = let
        val readEinUni = Controls.get Ctl.readEinUni
        val readEinLatex = Controls.get Ctl.readEinLatex
        val readEinRewrite = Controls.get Ctl.readEinRewrite

        (* ************** distribute and push Summation*********** *)
        val ein = EinSums.transform ein
        val lname = DstIR.Var.name  lhs
        val anames = List.map (fn v=> SrcIR.Var.name(v)) args

        (* ************** fixed *********** *)
        val level = 2
        val readEinPDF = true
        (* ************** function call *********** *)
        val _ = readEinSingleT (lname,anames,ein,level,s, readEinPDF, readEinUni,readEinLatex, readEinRewrite)

        in 1 end
    | expand (lhs, [SrcIR.EINAPP(ein, args)]) = let
        (*no string in argument. uses default filename(if argument given) or prints to screen*)
        val readEinUni = Controls.get Ctl.readEinUni
        val readEinLatex = Controls.get Ctl.readEinLatex
        val readEinRewrite = Controls.get Ctl.readEinRewrite
        val readEinPDF = Controls.get Ctl.readEinPDF
        (* ************** distribute and push Summation*********** *)
        val ein = EinSums.transform ein
        val lname = DstIR.Var.name  lhs
        val anames = List.map (fn v=> SrcIR.Var.name(v)) args

        (* ************** fixed *********** *)
        val level = 2
        val filename = "output_tmp"
        (* ************** function call *********** *)
        val _ = readEinSingleT (lname,anames,ein,level,filename, readEinPDF, readEinUni,readEinLatex, readEinRewrite)

in 1 end



  end (* HandleEin *)
