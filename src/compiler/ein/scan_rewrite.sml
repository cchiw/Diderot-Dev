(*rewriting for  friendly format*)

structure ScanRewrite : sig
    (*flaten, organize, and reduce*)
    val rewriteE : Ein.ein* bool  ->  Ein.ein
    (*last step refactoring*)
    val rewriteL : Ein.ein  ->  Ein.ein

    (*does multiple rewriting passes*)
    val scan_multiplePasses : ReadTy.readein*bool*string * Ein.ein * string list -> Ein.ein
    (*does single pass*)
    val scan_single :int *ReadTy.readein* bool*string * Ein.ein * string list -> Ein.ein
    val scan_pieces :ReadTy.readein* bool*string * Ein.ein * string list -> Ein.ein
  end = struct


    structure E = Ein
    structure Scan = ScanPP
    structure R = ReadTy

fun mkProd exps = E.Opn(E.Prod, exps)
fun mkDiv (e1, e2) = E.Op2(E.Div, e1, e2)
    fun getTops (name, es) = let
        fun f e = e
            val _ =  f(concat["\n", name,"-",Int.toString(length(es)),":"])
            val _ = List.map (fn e=> f("|"^EinPP.HeadexpToString(e))) es
        in 1 end

    fun getTopsPP (const,base, op1,op2,opn, rest) = let
        fun f e =    e
        val _ =  f("\n prod:")
        val _ = List.map (fn e=>f("|c-"^EinPP.HeadexpToString(e))) const
        val _ = List.map (fn e=>f("|b-"^EinPP.HeadexpToString(e))) base
        val _ = List.map (fn e=>f("|o1-"^EinPP.HeadexpToString(e))) op1
        val _ = List.map (fn e=>f("|o2-"^EinPP.HeadexpToString(e))) op2
        val _ = List.map (fn e=>f("|on-"^EinPP.HeadexpToString(e))) opn
        val _ = List.map (fn e=>f("|r-"^EinPP.HeadexpToString(e))) rest
        in 1 end



    fun organizeAdd es = let
    fun organize([], const,base, op1, op2, opn, rest) =
            (List.rev const, List.rev base,List.rev op1, List.rev op2,List.rev opn,List.rev rest)
        | organize(e1::es, const, base, op1, op2, opn, rest) = (case e1
            of E.Const _        =>  organize(es, e1::const, base, op1, op2, opn, rest)
            | E.Epsilon _       =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Eps2 _          =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Delta _         =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Probe _         =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Tensor _        =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Op1 _           =>  organize(es, const, base,  e1::op1, op2, opn, rest)
            | E.Op2 _           =>  organize(es, const, base, op1,  e1::op2, opn, rest)
            | E.Opn _           =>  organize(es, const, base, op1, op2,  e1::opn, rest)
            | _                 =>  organize(es, const, base, op1, op2, opn,  e1::rest)
            (*end case*))
        in organize(es, [], [], [], [], [],[]) end


    fun organizeProd es = let
        fun organize([], const,base, op1, op2, opn, rest) =
            (List.rev const, List.rev base, List.rev op1, List.rev op2, List.rev opn, List.rev rest)
        | organize(e1::es, const, base, op1, op2, opn, rest) = (case e1
            of  E.Const 1        =>  organize(es, const, base, op1, op2, opn, rest)
            | E.Const _        =>  organize(es, e1::const, base, op1, op2, opn, rest)
            | E.Epsilon _       =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Eps2 _          =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Delta _         =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Probe _         =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Tensor _        =>  organize(es, const,  e1::base, op1, op2, opn, rest)
            | E.Op1(E.Neg,e1)   =>  organize(e1::es, (E.Const ~1)::const, base,  op1, op2, opn, rest)
            | E.Op1 _           =>  organize(es, const, base,  e1::op1, op2, opn, rest)
            | E.Op2 (E.Div, E.Const 1, _)
                =>  organize(es, const, base, op1,  e1::op2, opn, rest)
            | E.Op2 (E.Div, eN, eD)
                =>  organize(eN::es, const, base, op1,  (E.Op2 (E.Div, E.Const 1, eD))::op2, opn, rest)
            | E.Op2 _          =>  organize(es, const, base, op1,  e1::op2, opn, rest)
            | E.Opn (E.Prod,ps) =>  organize(ps@es, const, base, op1, op2,  opn, rest)
            | E.Opn _           =>  organize(es, const, base, op1, op2,  e1::opn, rest)
            | _                 =>  organize(es, const, base, op1, op2, opn,  e1::rest)
            (*end case*))
        in organize(es, [], [], [], [], [],[]) end

    fun mkProd [] = E.Const 1
      | mkProd [e1] = e1
      | mkProd es = E.Opn(E.Prod, es)
    fun mkAdd [] = E.Const 0
      | mkAdd [e1] = e1
      | mkAdd es = E.Opn(E.Add, es)

    fun checkDiv(es1, es2, flagmatch) =
        let

            fun match([], eN, [], eD,changed) = (List.rev eN, List.rev eD,changed)
              | match(eAs, eN, [], eD,changed) = ((List.rev eN)@eAs, List.rev eD,changed)
              | match([], eN, eB::eBs, eD,changed) = match(List.rev eN, [], eBs, eB::eD,changed)
              | match(eA::eAs, eN, eB::eBs, eD,changed) =
                if(EinUtil.sameExp(eA,eB))
                then  (match(eAs, eN, eBs, eD,true))
                else match(eAs, eA::eN, eB::eBs, eD,changed)

            val (eN,eD,changed) = if(flagmatch) then match(es1, [], es2, [],false) else (es1,es2, false)
            val l1 = length(es1)
            val l2 = length(eN)
            val _ = if (l1>l2) then
                    (concat["\nNumerator:",Int.toString(l1),"-",Int.toString(l2), "denom :",Int.toString(length(es2)),Int.toString(length(eD))]) else   ""
            val e1 = mkProd eN
            val e2 = mkProd eD
        in (E.Op2(E.Div, e1, e2),changed) end


        fun checkAdd (es, flagmatch) = let
            fun flat([], done,changed) = (List.rev done,changed)
            | flat(E.Opn(E.Add, ps)::es, done,changed) = flat(ps@es, done, true)
            | flat(e1::es, done,changed) =flat(es, e1::done,changed)

            fun match([], done,changed) = (List.rev done,changed)
            | match(e1::es, done,changed) = let
                val (name1) = EinPP.expToString(e1)
                val _ = (concat["\n\nlooking for:\n",name1])
                    fun iter([],_) = match(es, e1::done,changed)
                  | iter (e2::rest, done') =
                        let
                            val name2 = EinPP.expToString(e2)
                            val _ = (concat["\n\tcompare:",name2])
                        in  if(EinUtil.sameExp(e1,e2))
                            then let
                                val e =  E.Opn(E.Prod, [E.Const 2, e2])
                                in match(es, (List.rev done')@(e::rest), true) end
                            else iter (rest, e2::done')
                        end
                in iter(done,[]) end
            val (es1, changed)  = flat (es,[], false)
            val (es',changed) = if(flagmatch) then match(es1, [], changed) else (es1, changed)
            val _ = (concat["\nAddition:",Int.toString(length(es)), "-",Int.toString(length(es1)),"-",Int.toString(length(es'))])
            val _ =  getTops("add", es')
            in (mkAdd es',changed) end


    fun rewriteE (Ein.EIN{body, params,index}, flagmatch) = let
        val changed = ref false
        val movedsum = ref false

        fun check(e,c) = if (c) then (changed:=true;e) else e
        fun rewrite(e)= ( ("\nin rewrite: "^EinPP.HeadexpToString(e)) ;case e
            of E.Opn(E.Add,[e1]) => e1
            | E.Opn(E.Add,es) => check(checkAdd (List.map rewrite es, flagmatch)) (* E.Opn(E.Add, List.map rewrite es)*)
            | E.Opn(E.Prod, [e1]) =>  e1
            | E.Opn(E.Prod, es) =>
                let

                   val _ =  getTops("prod", es)
                (*flatten constants*)
                fun flattenC ([], rest) =  List.rev rest
                  | flattenC (E.Const 1::es, rest)  =    flattenC(es,rest)
                  | flattenC (E.Const c1::E.Const c2::es, rest)= flattenC(E.Const(c1*c2)::es, rest)
                  | flattenC(e1::es, rest) = flattenC(es, e1::rest)

                fun flatOp1 ([], rest) =  List.rev  rest
                  | flatOp1 (E.Op1(E.Sqrt, e1)::E.Op1(E.Sqrt, e2)::es, rest)  =
                        if(flagmatch)
                            then if(EinUtil.sameExp(e1,e2))
                            then ( "found here";flatOp1(e1::es,rest))
                            else ( "did not find";flatOp1(E.Op1(E.Sqrt, e2)::es, E.Op1(E.Sqrt, e1)::rest))
                        else (flatOp1(E.Op1(E.Sqrt, e2)::es, E.Op1(E.Sqrt, e1)::rest))
                  | flatOp1(e1::es, rest) = flatOp1(es, e1::rest)
                (*flatten op2*)
                fun flatOp2(E.Op2(E.Div,E.Const 1, e2)::es, denom, subt, rest) = flatOp2(es, e2::denom, subt, rest)
                  | flatOp2(E.Op2(E.Div, e1, e2)::es, denom,subt, rest) = flatOp2(e1::es, e2::denom, subt,rest)
                  | flatOp2(E.Op2(E.Sub, e1, e2)::es, denom,NONE, rest) = flatOp2(es, denom, SOME(E.Op2(E.Sub, e1, e2)),rest)
                  | flatOp2(e1::es, denom, subt, rest) = flatOp2(es, denom, subt,e1::rest)
                  | flatOp2([], denom, subt, rest) = (List.rev denom,  subt, List.rev rest)
                (*flatten opn*)
                fun flatOpn(E.Opn(E.Add, ps)::es, rest) = (ps, List.rev(rest)@es)
                  | flatOpn(E.Opn(E.Prod, ps)::es, rest) = flatOpn(ps@es, rest)
                  | flatOpn(e1::es, rest) = flatOpn(es, e1::rest)
                  | flatOpn([], rest) = ([], List.rev rest)
                (*flatten product*)
                fun flatten [] = E.Const 1
                  | flatten [r]  = r
                  | flatten  rest =  E.Opn(E.Prod, rest)


                (*Organize product expression*)
                val es = List.map rewrite es
                val  (const, base, op1, op2, opn, rest) = organizeProd es
                (*flatten operators t*)
                val const = flattenC(const, [])
                val op1 = flatOp1(op1, [])

                val (denom, subt, op2) = flatOp2(op2, [],NONE, [])
                val (add, opn) = flatOpn (opn,[])
                val pp = const@base@op1@op2@opn@rest
                 val _ =  getTopsPP(const,base,op1,op2,opn,rest)
                val done = flatten pp


                (*apply denominator in division*)
                val done =(case denom
                    of  []    => done
                    | [denom] => (changed:=true;E.Op2(E.Div, done, denom))
                    | denom   =>   (changed:=true;E.Op2(E.Div, done,  E.Opn(E.Prod,  denom)))
                    (*end case*))

                val done = (case (add, done)
                    of ([],_) => done
                    | (_ , E.Opn(E.Prod, es)) =>  (changed:=true;E.Opn(E.Add, List.map (fn e2 =>  (E.Opn(E.Prod, e2::es))) add))
                    |  _ =>    (changed:=true;E.Opn(E.Add, List.map (fn e2 =>  (E.Opn(E.Prod, [e2,done]))) add))
                    (*end case*)) (*^ need to add cases here*)
                val done = (case (subt,done)
                    of (NONE,_) => done
                    | (SOME(E.Op2(E.Sub, e1, e2)),  E.Opn(E.Prod, es))
                        =>  (changed:=true;E.Op2(E.Sub,E.Opn(E.Prod, [e1,done]),E.Opn(E.Prod, e2::es)))
                    | (SOME(E.Op2(E.Sub, e1, e2)), _)
                        =>  (changed:=true;E.Op2(E.Sub,E.Opn(E.Prod, [e1,done]),E.Opn(E.Prod, [e2,done])))
                    (*end case*))
                in done end

            | E.Op1(E.PowInt 2, E.Op1(E.Sqrt, e1)) => (changed:=true; e1)
            | E.Op1(op1, e1) => E.Op1(op1, rewrite e1)

            | E.Op2(E.Div, E.Op2(E.Div, a, b), E.Op2(E.Div, c, d)) => (changed:=true;rewrite (mkDiv (mkProd[a, d], mkProd[b, c])))
            | E.Op2(E.Div, E.Op2(E.Div, a, b), c) => (changed:=true;rewrite (mkDiv (a, mkProd[b, c])))
            | E.Op2(E.Div, a, E.Op2(E.Div, b, c)) => (changed:=true;rewrite (mkDiv (mkProd[a, c], b)))
            | E.Op2(E.Div, a, b) => let
                val _ = getTops("div", [a,b]);
                in
                (case (rewrite a, rewrite b)
                    of (E.Opn(E.Prod, es1), E.Opn(E.Prod, es2))
                        =>  check( checkDiv(es1, es2, flagmatch))
                    | (E.Opn(E.Prod, es1), e2) => check( checkDiv(es1, [e2], flagmatch))
                    | (e1, E.Opn(E.Prod, es2)) => check(checkDiv([e1], es2, flagmatch))
                    |  (E.Opn(E.Add, es), eD) => let

                        fun iter([], done) =   (changed:=true;E.Opn(E.Add, List.rev done))
                          | iter (E.Op2(E.Div, e1, e2)::rest, done) =
                            (changed:=true; iter(rest, E.Op2(E.Div, e1, rewrite(E.Opn(E.Prod,[e2,eD])))::done))
                          | iter(e1::rest, done) = iter(rest, E.Op2(E.Div, e1, eD)::done)
                        in iter(List.map rewrite es, []) end


                    | (e1,e2)                  => check(checkDiv([e1], [e2], flagmatch))
                (*end case *))
                end
            | E.Op2(E.Sub, E.Op2(E.Sub, e1, e2),E.Op2(E.Sub, e3, e4)) => let
                val p2 = E.Opn(E.Prod,[ E.Const ~1, e2])
                val p3 = E.Opn(E.Prod,[ E.Const ~1, e3])
                in (changed:=true; E.Opn(E.Add,[e1,p2,p3,e3])) end
            | E.Op2(E.Sub, e1, e2)=>
                if(EinUtil.sameExp(e1,e2)) then  (changed:=true;  E.Const 0)
                else ( E.Opn(E.Add,[rewrite e1,  E.Opn(E.Prod,[ E.Const ~1, rewrite e2])]))
            | E.Op2(op2, e1, e2)=>      (getTops("op2"^EinPP.HeadexpToString(e), [e1,e2]); E.Op2(op2, rewrite e1, rewrite e2))

            | E.Sum(sx, sumexp) => let

                    val _ =  getTops("summation", [sumexp])

                fun f e1 =  (changed:=true; movedsum:=true; e1)
                in
                    (case (rewrite sumexp)
                        of E.Opn(E.Add, es) =>  f (E.Opn(E.Add, List.map (fn e1=> E.Sum(sx, e1)) es))
                        | E.Op1(op1, e1) => f(E.Op1(op1, E.Sum(sx,e1)))
                        | E.Op2(op2, e1,e2) => f(E.Op2(op2, E.Sum(sx,e1), E.Sum(sx,e2)))
                        | sumexp => ( ("\nsum else: "^EinPP.HeadexpToString(e)) ; E.Sum(sx, sumexp))
                    (*end case*))
                end
            | _ => e
            (* end case*))

        fun loop(body,n) = let
            val _ = (concat["\n\nIteration:", Int.toString(n)])
            val body = (changed:=false;rewrite body)
            in
                if(!changed)
                then loop(body,n+1)
                else
                    if(!movedsum)
                    then let
        val _ = "inside moved sum"
                        val body = EinSums.clean_body(EinSums.distribute_body(body))
                        in (movedsum:=false;loop(body,n+1))
                        end
                    else body
            end
        val body= loop(body,0)
        val _ = "done loop"
        in Ein.EIN{body=body, params=params, index=index}
        end

    (* last rewriting before we go to ing*)
    (*attempt some refactoring*)
    fun rewriteL (Ein.EIN{body, params,index}) = let
                val changed = ref false
            (*grouping positive and negative terms together*)
            fun orgAdd es  = let
                fun iter([], pos, []) = mkAdd pos
                | iter([], [], neg) = E.Op1(E.Neg, mkAdd neg)
                | iter([], pos, neg) =  E.Op2(E.Sub, mkAdd pos, mkAdd neg)
                | iter(E.Op1(E.Neg, e1)::es, pos, neg) = iter(es, pos, e1::neg)
                | iter(e1::es, pos, neg) = iter(es, e1::pos, neg)
                in iter(es,[], []) end



        fun rewrite e = (case e
            of E.Opn(E.Prod, E.Const ~1::es) =>  (changed:=true; E.Op1(E.Neg, E.Opn(E.Prod,es)))
            | E.Op1(op1, e1) =>  E.Op1(op1, rewrite e1)
            | E.Op2(op2, e1,e2) =>  E.Op2(op2, rewrite e1, rewrite e2)
            | E.Opn(E.Add, [e1]) => e1

            | E.Opn(E.Add, E.Op2(E.Div, e1,e2)::es) => let
                val _ = ("\n  scan 1 length of addition list"^Int.toString(length(E.Op2(E.Div, e1,e2)::es)))
                val e1 = rewrite e1
                val e2 = rewrite e2
                (*factor out common denominator*)
                fun iter([], num, rest) =  orgAdd  (E.Op2(E.Div, mkAdd num, e2)::rest)
                  | iter (E.Op2(E.Div, e3,e4)::es, num, rest) =
                    if (EinUtil.sameExp(e2,e4))
                    then  (changed:=true; iter(es, e3::num, rest))
                    else iter(es, num, E.Op2(E.Div, e3,e4)::rest)
                  | iter(e1::es, num, rest) = iter(es, num, e1::rest)

                val es = List.map rewrite es
                in (case (rewrite (E.Opn(E.Add, es)))
                    of E.Opn(E.Add, es) => iter(es, [e1], [])
                    | e3 =>  orgAdd  [E.Op2(E.Div, e1,e2), e3]
                    (*end case*))
                end

            | E.Opn(E.Prod, e1::es) => let
                val e1 = rewrite  e1
                val es = List.map rewrite es
                in (case (E.Opn(E.Prod, es))
                    of E.Opn(E.Prod, es) => mkProd (e1::es)
                    | e2 => mkProd [e1,e2]
                    (* end case*))
                end
            | E.Opn(E.Add, e1::es) => let
                val _ = ("\n scan 2 length of addition list"^Int.toString(length(e1::es)))
                val e1 = rewrite  e1
                val es = List.map rewrite es
                in (case (E.Opn(E.Add, es))
                    of E.Opn(E.Add, es) => orgAdd (e1::es)
                    | e2 => orgAdd [e1,e2]
                    (* end case*))
                end


            | E.Sum(sx, e) => E.Sum(sx, rewrite e)
            | _  => e
        (* end case*))
        fun loop(body,n) = let
            val _ = (concat["\n\nIteration:", Int.toString(n)])
            val body = (changed:=false;rewrite body)
            in
                if(!changed)
                then loop(body,n+1)
                else body
            end
        val body= loop(body,0)
        val body= loop(body,0)
        in Ein.EIN{body=rewrite body, params=params, index=index}
        end

(* ------------------------------------------------------------------------------- *)




val reader = Scan.scanEin

(*name of rewriting pass*)
fun rewriteName 1 = "flat"
| rewriteName 2 =  "match"
| rewriteName 3 = "refactor"
| rewriteName 4 = "pull-division"
| rewriteName 5 = "tab addition"
(*apply just once*)
fun apply (1,e) = EinSums.transform (rewriteE (e,false))  (*flatten*)
  | apply (2,e) = EinSums.transform (rewriteE (e,true))   (*look for matching terms*)
  | apply (3,e) = rewriteL e (*does some refactoring*)
  | apply (_,e) = e

(*do multiple rewriting passes*)
    fun build(0,e) = e
    | build (n,e) = apply(n, build(n-1,e))

fun makesurface(5, op_n, anames,lname, exp) = reader(op_n,anames,lname, exp, true,true)
| makesurface(4, op_n, anames,lname, exp) = reader(op_n,anames,lname, exp, true,false)
| makesurface(_, op_n, anames,lname, exp) = reader(op_n,anames,lname, exp, false,false)

fun psurface(level, readEinLatex, surface) = let
    val name = rewriteName level
    in
    if readEinLatex
        then (concat["\n\\\\\n\\newline $\\rightarrow_{", name,"}", surface,"$"])
        else (concat["\n\n ",name,"\t->",surface])
    end


fun cvt_single  (level, op_n,readEinLatex,lname, e, anames) =
    let
        val exp = build(level,e)
        val surface = makesurface(level, op_n, anames,lname, exp)
        val nsurface = psurface (level, readEinLatex, surface)
    in (exp,nsurface) end

(*prints out pieces*)
fun scan_single  (level, op_n,readEinLatex,lname, e, anames) =
    let
        val (exp, nsurface) = cvt_single  (level, op_n,readEinLatex,lname, e, anames)
        val _ = print (concat[lname, nsurface, "\n"])
    in exp end
(*for spllit*)
fun scan_pieces  (op_n,readEinLatex,lname, e, anames) =
    let
        val level = 3
        val (exp, nsurface) = cvt_single  (level, op_n,readEinLatex,lname, e, anames)
        (*
        val lnamel = String.size(lname)
        val lA = StringCvt.padLeft #" " (15-lnamel)  "= "
        val tmp = if readEinLatex then concat["\\newline ",lname, lA, "$",surface,"$"]
                    else concat[lname, lA, surface]
        val lC = StringCvt.padLeft #" " (45-size(tmp))  " "
        val _ = print(concat[tmp,lC,"\n"])*)
        val _ = print (concat[lname, nsurface, "\n"])
    in  exp end
(*does multiple rewriting passes*)
fun scan_multiplePasses (op_n,readEinLatex,lname, e, anames) =
    let
        fun iter (5,exp, es) = (exp, List.rev es) (*stop at last step*)
          | iter (level,exp,es) = let
            val (exp, nsurface) = cvt_single  (level, op_n,readEinLatex,lname, e, anames)
            in iter(level+1,exp, nsurface::es) end

        val (exp, es) = iter(1,e, [])
        val _ = print (concat[lname, concat(es),"\n"])

    in  exp end

end
