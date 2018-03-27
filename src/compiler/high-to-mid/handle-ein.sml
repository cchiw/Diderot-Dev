(* handle-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure HandleEin : sig

    val expand : MidIR.var * Ein.ein * MidIR.var list -> MidIR.assignment list

  end = struct
   
    structure E = Ein
    structure SrcIR = HighIR
    structure DstIR = MidIR
    structure ScanR= ScanRewrite
    structure R = ReadTy


    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt
    fun ll ([],cnt) = ""
    | ll (a1::args,cnt) = String.concat["\n\t", Int.toString(cnt),"_", MidTypes.toString(DstIR.Var.ty a1), " ", MidIR.Var.name(a1),",", ll(args,cnt+1)]



    fun prntNewbies(newbies, id) = let
        val _ = (id)
        in  List.map (fn (lhs,DstIR.EINAPP(e,a))=> (concat["\n\n ->:", MidTypes.toString(DstIR.Var.ty lhs)," ",DstIR.Var.name(lhs),"=",EinPP.toString(e) ,"-",ll(a,0),"---->"])  | _ => "") newbies
        end
    val flagmatch = true

    fun expand (lhs, ein, args) = let
        val _ = ("\n\n   ***********\n")
         val _ = (String.concat["\n\n   expand ***********\n:",MidIR.Var.name(lhs),"=", EinPP.toString(ein),"-",ll(args,0)])
        val sliceFlag = Controls.get Ctl.sliceFlag
        val fullSplitFlag = Controls.get Ctl.fullSplitFlag
        val replaceFlag =  Controls.get Ctl.replaceFlag


        (*reading ein controls*)
        (*what do we read?*)
        val readEinRewrite = Controls.get Ctl.readEinRewrite
        val readEinSplit = Controls.get Ctl.readEinSplit
        val readEin1 = Controls.get Ctl.readEin1
        val readEin2 = Controls.get Ctl.readEin2
        val readEin3 = Controls.get Ctl.readEin3
        val readEin4 = Controls.get Ctl.readEin4
        val readEin5 = Controls.get Ctl.readEin5
        (*what format do we use to print htings out*)
        val readEinUni = Controls.get Ctl.readEinUni
        val readEinLatex = Controls.get Ctl.readEinLatex
        val readEinWord = Controls.get Ctl.readEinWord
        (*use latex by default*)
        val op_n = if readEinUni then R.op_u  else if readEinWord then  R.op_w else R.op_l

        fun scan_multiplePasses (lhs,DstIR.EINAPP(e,a)) = let
            val e = ScanR.scan_multiplePasses (op_n,readEinLatex, DstIR.Var.name  lhs, e, List.map (fn v=> DstIR.Var.name(v)) a)
            in (lhs,DstIR.EINAPP(e,a)) end
          | scan_multiplePasses e = e
        fun scan_single (level, (lhs,DstIR.EINAPP(e,a))) = let
            val e = ScanR.scan_single (level, op_n, readEinLatex,DstIR.Var.name  lhs, e, List.map (fn v=> DstIR.Var.name(v)) a)
            in (lhs,DstIR.EINAPP(e,a)) end
          | scan_single (_,e) = e
        fun scan_pieces (lhs,DstIR.EINAPP(e,a)) = let
            val e = ScanR.scan_pieces (op_n, readEinLatex,DstIR.Var.name  lhs, e, List.map (fn v=> DstIR.Var.name(v)) a)
            in (lhs,DstIR.EINAPP(e,a)) end
        | scan_pieces  e = e



        (* ************** distribute and push Summation*********** *)
          val ein' = EinSums.transform ein
        (*clean parametes*)
        val Ein.EIN{body, params,index} = ein'
        val DstIR.EINAPP(ein', args) = CleanParams.clean (body, params, index, args)

        (* ************** Scan and rewrite *********** *)
        val newbie = (lhs, DstIR.EINAPP(ein', args))
        val newbie = if(readEinRewrite) then scan_multiplePasses (newbie) else newbie
        val newbie = if(readEin1)  then scan_single (1, newbie) else newbie
        val newbie = if(readEin2)  then scan_single (2, newbie) else newbie
        val newbie = if(readEin3)  then scan_single (3, newbie) else newbie
        val newbie = if(readEin4)  then scan_single (4, newbie) else newbie
        val newbie = if(readEin5)  then scan_single (5, newbie) else newbie
        val newbie = if(readEinSplit)  then  scan_pieces (newbie) else newbie

        (* **************** split phase ************* *)
          fun iter([], ys) = ys
            | iter(e1::es, ys) = let
            val (lhs, DstIR.EINAPP(e, a)) = e1
            val y1 = FloatEin.transform (lhs, e, a)
            in
                iter(es, ys@y1)
            end
        val _ =  "about to split"
        val newbies =iter([newbie], [])
        (* val _ = print(concat["\n -----> Pieces: ",Int.toString(length(newbies)),"\n"])*)
        (*val _ = prntNewbies(newbies, "\n\n\npost floatx1")*)
        val newbies =iter(newbies, [])
        (*val _ = prntNewbies(newbies, "\n\n\npost floatx2")*)
        val _ =  if(readEinSplit) then List.map scan_pieces  newbies else newbies
        val _ = (String.concat["Number of pieces:",Int.toString(length(newbies))])

        (* **************** expand-fem ************* *)
        (*val _ =  "\n\n\nbefore translating fields"*)
        val newbies  = List.foldr (fn (e,acc)=>  translateField.transform(e)@acc ) []  newbies
        (*val _ = prntNewbies(newbies, "\n\n\npost transform fields")*)
        (* ************** ProbeEIN *********** *)
        (*   val _ = "about to call probe ein"*)
        val avail = AvailRHS.new()
        val _ = List.app (fn n1 => ProbeEin.expand(avail,  n1)) newbies;
        val stmts = List.rev (AvailRHS.getAssignments avail)
        val asgn = List.map DstIR.ASSGN stmts
        val _  = "exit"
          in
            asgn
          end

  end (* HandleEin *)
