

structure ScanPP : sig

    val scanEin : ReadTy.readein*string list*string * Ein.ein *  bool*  bool -> string
    val cvtId : string list*int -> string


  end = struct


    structure E = Ein
    structure R = ReadTy

    (* ------------------------------------------------------------------ *)
    fun alphaToStr([]) = ""
    | alphaToStr ((E.V v1)::es) = concat["V", Int.toString(v1), alphaToStr(es)]
    | alphaToStr ((E.C v1)::es) = concat["C", Int.toString(v1), alphaToStr(es)]
    fun cvtId(args,id) = List.nth(args, id)

    (* ------------------------------------------------------------------ *)

    fun mkPow(op_n,ss,n) = concat["(",ss,")",R.get_op_pown(op_n),Int.toString(n)] (**)

    fun mkDet2(ss) = concat["Det2(",ss,")"]
    fun mkTrace (s1) = concat["Trace(",s1,")"]
    fun mkReduce (s1) = concat["SumVec(",s1,")"]



    fun mkTraversedI(s1, s2) =
        concat["Trav(",s1,")<",String.concatWith"," (List.map Int.toString s2), ">"]
    fun mkTraversedS(s1, s2) = concat["Trav(",s1,")<", s2, ">"]
    fun mkSlice(s1, s2) = concat["(",s1,"[",s2,"])"]
    fun mkMod(s1, s2) = concat["modulate(",s1, ",",s2,")"]
    fun mkUnary(op_n,op1) = (case op1
        of E.Neg => "-"
        | E.Exp => "exp"
        | E.Sqrt   => R.get_op_sqrt(op_n)
        | E.Cosine =>  "cos"
        | E.ArcCosine => "acos"
        | E.Sine    => "sine"
        | E.ArcSine => "asine"
        | E.Tangent => "tan"
        | E.ArcTangent => "atan"
        | E.Abs => "abs"
        | E.Sgn => "sign"
        | _ => "Error"
        (**))

    fun mkTranspose(op_n,ss) = concat["(",ss,")",R.get_op_transpose(op_n)]
    fun mkSubt(op_n,s1, s2) = concat["(",s1,R.get_op_subt(op_n),s2,")"]
    fun mkDouble(op_n,s1, s2) = concat["(",s1,R.get_op_double(op_n),s2,")"]
    fun mkScale(op_n,s1, s2) = concat["(",s1,R.get_op_scale(op_n),s2,")"]
    fun mkInner(op_n,s1, s2) = concat["(",s1,R.get_op_inner(op_n),s2,")"]
    fun mkCross(op_n,s1, s2) = concat["(",s1,R.get_op_cross(op_n),s2,")"]
    fun mkOuter(op_n,s1, s2) = concat["(",s1, R.get_op_otimes(op_n),s2,")"]
    fun mkDivide(op_n,s1,s2,flag_pulldiv) =
        if(flag_pulldiv)
        then concat[s1,R.get_op_scale(op_n),R.get_op_divide(op_n),"{1}{",s2,"}"]
        else concat[R.get_op_divide(op_n),"{",s1, "}{",s2,"}"]

    fun cvtdx (op_n,dx) = let
        val op_deriv = R.get_op_deriv(op_n)
        val op_otimes=  R.get_op_otimes(op_n)
        fun f []  = ""
            | f [_] = op_deriv
            | f [_,_] = concat[op_deriv, op_otimes, op_deriv]
            | f (_::es) = concat[op_deriv, op_otimes,  f(es)]
        in f(dx) end
        
    (* ------------------------------------------------------------------ *)
    (*returns bools*)
    fun isOrdered([], n) = true
    | isOrdered ((E.V v1)::es, n) = if(v1=n) then isOrdered(es, n+1) else false
    | isOrdered (_,n) = false

    fun sort [v0, v1] =  if(v0<v1) then [0, 1] else  [1,0]
     |  sort [v0,v1,v2] =
        if(v0<v1) then
            if(v1<v2) then [0,1,2]                           (*v0<v1<v2*)
            else
                if(v0<v2) then [0,2,1]                       (*v0<v2<v1*)
                else [2,0,1]                                 (*v2<v0<v1*)
        else
            if(v0<v2)   then [1,0,2]                         (*v1<v0<v2*)
            else
                if(v2<v1) then [2,1,0]                       (*v2<v1<v0*)
                else [1,2,0]                                 (*v1<v2<v0*)
    | sort [v0,v1,v2,v3] =
        if(v0<v1 andalso v1<v2 andalso v2<v3) then [0,1,2,3]                   (*v0<v1<v2<v3*)
        else if(v0<v1 andalso v1<v3 andalso v3<v2)                                (*v0<v1<v3<v2*)
            then ([0,1,3,2])
        else if(v0<v2 andalso v2<v1 andalso v1<v3)                                (*v0<v2<v1<v3*)
            then ([0,2,1,3])
        else if(v0<v2 andalso v2<v3 andalso v3<v1)                                (*v0<v2<v3<v1*)
            then ([0,2,3,1])
        else if(v0<v3 andalso v3<v1 andalso v1<v2)                                (*v0<v3<v1<v2*)
            then ([0,3,1,2])
        else if(v0<v3 andalso v3<v2 andalso v2<v1)                                (*v0<v3<v2<v1*)
            then ([0,3,2,1])
        else if(v1<v0 andalso v0<v2 andalso v2<v3) 				      (*v1<v0<v2<v3*)
            then ([1,0,2,3])
        else if(v1<v0 andalso v0<v3 andalso v3<v2)                                (*v1<v0<v3<v2*)
            then ([1,0,3,2])
        else if(v1<v2 andalso v2<v0 andalso v0<v3)                                (*v1<v2<v0<v3*)
            then ([1,2,0,3])
        else if(v1<v2 andalso v2<v3 andalso v3<v0)                                (*v1<v2<v3<v0*)
            then ([1,2,3,0])
        else if(v1<v3 andalso v3<v0 andalso v0<v2)                                (*v1<v3<v0<v2*)
            then ([1,3,0,2])
        else if(v1<v3 andalso v3<v2 andalso v2<v0)                                (*v1<v3<v2<v0*)
            then ([1,3,2,0])
        else if(v2<v0 andalso v0<v1 andalso v1<v3) 				      (*v2<v0<v1<v3*)
            then ([2,0,1,3])
        else if(v2<v0 andalso v0<v3 andalso v3<v1)                                (*v2<v0<v3<v1*)
            then ([2,0,3,1])
        else if(v2<v1 andalso v1<v0 andalso v0<v3)                                (*v2<v1<v0<v3*)
            then ([2,1,0,3])
        else if(v2<v1 andalso v1<v3 andalso v3<v0)                                (*v2<v1<v3<v0*)
            then ([2,1,3,0])
        else if(v2<v3 andalso v3<v0 andalso v0<v1)                                (*v2<v3<v0<v1*)
            then ([2,3,0,1])
        else if(v2<v3 andalso v3<v1 andalso v1<v0)                                (*v2<v3<v1<v0*)
            then ([2,3,1,0])
        else if(v3<v0 andalso v0<v1 andalso v1<v2) 				      (*v3<v0<v1<v2*)
            then ([3,0,1,2])
        else if(v3<v0 andalso v0<v2 andalso v2<v1)                                (*v3<v0<v2<v1*)
            then ([3,0,2,1])
        else if(v3<v1 andalso v1<v0 andalso v0<v2)                                (*v3<v1<v0<v2*)
            then ([3,1,0,2])
        else if(v3<v1 andalso v1<v2 andalso v2<v0)                                (*v3<v1<v2<v0*)
            then ([3,1,2,0])
        else if(v3<v2 andalso v2<v0 andalso v0<v1)                                (*v3<v2<v0<v1*)
            then ([3,2,0,1])
        else                                (*v3<v2<v1<v0*)
            ([3,2,1,0])
    

    (*if possible make indices ordered *)
    fun mkOrdered(op_n,s1, alpha1) =  (case alpha1
        of [E.C c1, E.V v2] => (mkSlice(s1, Int.toString(c1)^",:"),[E.V v2])
        | [E.V v1, E.V v2] =>
            if(v1<v2) then (s1, alpha1)
            else    let
                        val _ = (String.concat["\n mktranspose from mkordered. thinks alpha is:",Int.toString(v1),"-",Int.toString(v2) ])
                    
                    in  (mkTranspose(op_n,s1), [E.V v2, E.V v1])
                    end
        | [E.V v0, E.V v1, E.V v2] =>
            (case  sort([v0,v1,v2])
                of [0,1,2] => (s1, alpha1)
                | [a,b,c] => let
                    val ordering = [a,b,c]
                    val shape = [(List.nth(alpha1, a)), (List.nth(alpha1, b)), (List.nth(alpha1, c))]
                    val sa = if(a=0) then ":" else Int.toString(a)
                    val sb = if(b=1) then ":" else Int.toString(b)
                    val sc = if(c=2) then ":" else Int.toString(c)
                    in  (mkTraversedS(s1, concat[sa,",",sb,",",sc]), shape) end
            (* end case*))
        | [E.V v0, E.V v1, E.V v2, E.V v3] =>
            (case sort([v0,v1,v2,v3])
                of [0,1,2,3] => (s1, alpha1)
                | [a,b,c,d] => let
                    val ordering = [a,b,c,d]
                    val shape = [List.nth(alpha1, a), List.nth(alpha1, b), List.nth(alpha1, c),List.nth(alpha1, d)]
                    val sa = if(a=0) then ":" else Int.toString(a)
                    val sb = if(b=1) then ":" else Int.toString(b)
                    val sc = if(c=2) then ":" else Int.toString(c)
                    val sd = if(d=3) then ":" else Int.toString(d)
                    in  (mkTraversedS(s1, concat[sa,",",sb,",",sc,",",sd]), shape) end
            (* end case*))
        | _ => (s1,alpha1)
        (* end case*))

    (* ------------------------------------------------------------------ *)

    fun iter(e1::e2::es) = if(e1=e2) then iter (es) else false
    | iter _ = true

    fun checkIndex([E.V v1], E.V n) = (v1=n)
      | checkIndex  _ = false

    (*is the index in the list*)
    fun checkIndices(alpha, vid) = let
        fun sort ((E.V v1)::es, location, rest) =
            if(v1=vid)  then SOME(location, rest@es) else sort(es, location+1, rest@[E.V v1])
        | sort _ = NONE
        in sort(alpha, 1,[]) end


    fun checkEps(vid1, vid2, j, k, alpha1,alpha2) =
        ((vid1= j) andalso checkIndex(alpha1, vid1) andalso (vid2= k) andalso checkIndex(alpha2,vid2))

(* ------------------------------------------------------------------ *)

    fun scan(op_n,args,body,flag_pulldiv,flag_shiftAdd) = let
        val complex = ref false
        val errterm = ref ""
        fun unhandled (id,e) =
            raise Fail("\n\nunhandled:"^id^" \nexp:"^EinPP.expToString(e)^"\n\n\n")
        fun checkSubt(e,s1, alpha1, s2, alpha2) =
            if (iter [alpha1,alpha2])
            then (mkSubt(op_n,s1, s2), alpha1)
                else let
                    val (s1, alpha1) = mkOrdered(op_n,s1, alpha1)
                    val (s2, alpha2) = mkOrdered(op_n,s2, alpha2)
                    val s  = ( concat["\nsubt:","\n\t",s1,"::",alphaToStr(alpha1),"\n\t",s2,"::",alphaToStr(alpha2),"\n\n"])
                    val _ =  s 
                    in if (iter [alpha1,alpha2])
                        then (mkSubt(op_n,s1, s2), alpha1)
                        else unhandled (s,e)
                    end


        (* get name of expression and list of indices*)
        fun getAlpha e =
        let
        
            (*get name and size*)
            fun wrap(name, e1)  = let
                val (s1, alpha1) =getAlpha(e1)
                in (concat[name,"(",s1,")"], alpha1) end
            fun wrapS (name, e1) = let
                val (s1, _) =getAlpha(e1)
                in (concat[name,"(",s1,")"], []) end
            fun wrapNorm e1 =let
                val (s1, _) = getAlpha(e1)
                in (concat["|",s1,"|"], []) end
        
            (*get alpha addition *)
            fun getAdd (e1::es) = let
            
                val _ = (concat["\n\n\n--Big Add--",EinPP.expToString(e)])
                val (s1, alpha1) = getAlpha (e1)
                val _ = (concat["\n\n\n--starting", s1, "\n\te:",alphaToStr(alpha1),"\n ein: ",EinPP.expToString(e1)])
                val (s1, alpha1) = mkOrdered(op_n,s1, alpha1)
                val _ =(concat["\n\n\n--ordered--",s1, "\n\t",alphaToStr(alpha1), "\n\t"])
                val _ = (concat["\n\n\n--starting add with elements",Int.toString(length(e1::es)),"--",s1, "\n\t"])
                fun sort(n,[], done) =
                        if(flag_shiftAdd) then
                                (concat["(",String.concatWith"\n\t+" (List.rev done),")"], alpha1)
                        else    (concat["(",String.concatWith"+" (List.rev done),")"], alpha1)
                  | sort(n, E.Opn(E.Add, es2)::es, done) =  sort(n, es2@es, done)
                  | sort (n,e2::es, done) = let
                    val (s2, alpha2) = getAlpha (e2)
                    val (s2, alpha2) = mkOrdered(op_n,s2, alpha2)
                    val tmp = (concat["\n\n\n--add",Int.toString(n),"--",s2, "\n\t expected: ", alphaToStr(alpha1),"\n\t observed: ", alphaToStr(alpha2), "\n\t",EinPP.expToString(e2),"\n\n\n"])
                    val _ =  (tmp)
                    in
                    if (iter ([alpha1,alpha2]))  then sort(n+1, es, s2::done)
                    else  if (iter ([alpha1,alpha2])) then sort(n+1, es, s2::done)
                            else unhandled ("41"^tmp,e)
                    end
                val (name, beta) = sort(0, es, [s1])
                val _ = ("\nAdd:"^name)
                in (name, beta)
                end
            (*get alpha of product*)
            fun getProd ps =    let
                val _ =   "\nproduct term"
                val _ = List.map (fn e=> ("|"^EinPP.HeadexpToString(e))) ps
                (*sort by type*)
                fun sort([], sca, ten) = (sca, ten)
                  | sort(e1::e2::es, sca, ten) = let
                    val (s1, alpha1) = getAlpha (e1)
                    val (s2, alpha2) = getAlpha (e2)
                    val _ = (String.concat["\n\t-",s1, alphaToStr(alpha1)])
                    val _ = (String.concat["\n\t-",s2, alphaToStr(alpha2)])
                    in (case (alpha1,alpha2)
                        of ([],[]) => let
                            (* check if both scalar expressions are the same e*e*)
                            val term =
                                if(EinUtil.sameExp(e1, e2)) then [mkPow(op_n,s1, 2)] else [s2, s1]
                                in sort(es, term@sca, ten) end
                        | ([],_) => sort(e2::es, s1::sca, ten)
                        | _      => sort(e2::es, sca, (s1, alpha1)::ten)
                        (* end case*))
                    end
                | sort(e1::es, sca, ten) = let
                    val (s1, alpha1) = getAlpha (e1)
                    in (case alpha1
                        of [] => sort(es, s1::sca, ten)
                        | _ =>   sort(es, sca, (s1, alpha1)::ten)
                        (* end case*))
                    end
                (*flatten tensor terms*)
                fun flat(sca, (s2,a2 as E.V v2::_)::(s3, a3  as E.V v3::_)::ten) =
                    let
                        (*flatten operator between two tensors*)
                        val (name, shape) =
                            if(isOrdered (a2,v2)  andalso isOrdered (a3,length(a2)))
                            then   (mkOuter(op_n,s2, s3), a2@a3)
                            else if(isOrdered (a3,v3)  andalso isOrdered (a2,length(a3)))
                            then   (mkOuter(op_n,s3, s2), a3@a2)
                            else if(a2=a3) then   (mkMod(s2, s3), a2)
                            else unhandled ("812",E.Opn(E.Prod,ps))
                                (*(mkScale(op_n,s2, s3), a2@a3)  (*maybe she be an error*)*)
                            in flat(sca, (name,shape)::ten) end
                    | flat(sca, [(s2,a2)]) = (*0 or 1 tensor*)
                    (concat["(",String.concatWith (R.get_op_scale(op_n)) (List.rev (s2::sca)),")"],a2)
                    | flat(sca, []) = (*0 or 1 tensor*)
                    (concat["(",String.concatWith (R.get_op_scale(op_n)) (List.rev (sca)),")"],[])
                val (sca, ten) = sort(ps, [],[])
                val (name, beta) = flat(sca,ten)
                val _ = (String.concat["\n mkProduct:", name, alphaToStr(beta)])
                in (name,beta) end
            fun run(e) = (case e
                of E.Epsilon(i,j,k)         => (*("EPS3", [i,j,k])*)  unhandled("EPS3",e)
                | E.Eps2(i,j)               => (*("EPS2", [i,j])*)    unhandled("EPS2",e)
                
                | E.Delta(i,j)              => ("Identity", [i,j])
                | E.Tensor (id, alpha)      => (cvtId(args,id), alpha)
                | E.Field (id, alpha)       => (cvtId(args,id), alpha)
                | E.Conv (id,alpha, hid,[]) =>  (cvtId(args,id), alpha)
                | E.Conv (id,[], hid, dx) =>
                    (*scalar with derivative*)
                    (concat[cvtdx(op_n, dx),cvtId(args,id)], dx)
                | E.Conv (id,alpha, hid, dx) =>
                    let (*shape with derivativ*)
                        val name = concat[cvtdx(op_n, dx),R.get_op_otimes(op_n)]
                    in   (concat[name,cvtId(args,id)], alpha@dx)
                                            end
                | E.Opn(E.Prod, E.Const ~1::es) => getAlpha( E.Op1(E.Neg, E.Opn(E.Prod,es)))
                | E.Zero [v] => ("ZeroV", [v])
                | E.Zero [u,v] => ("ZeroV", [u,v])
                | E.Comp(e1, [(e2,_)]) => let
                    val (s1, beta1) = getAlpha (e1)
                    val (s2, _) = getAlpha (e2)
                    val name = concat[s1,R.get_op_comp(op_n),s2]
                    in (name, beta1)
                    end
                | E.Comp(e1, [(e2,_),(e3,_)]) => let
                    val (s1, beta1) = getAlpha (e1)
                    val (s2, _) = getAlpha (e2)
                    val (s3, _) = getAlpha (e3)
                    val name = concat[s1,R.get_op_comp(op_n),s2,R.get_op_comp(op_n),s3]
                    in (name, beta1)
                    end
                | E.Apply(E.Partial dx, e2) => let
                    val (s2, alpha2) =getAlpha(e2)
                    val dxname = cvtdx(op_n, dx)
                    val name =(case alpha2
                        of [] =>  concat[dxname,s2]
                        | _ =>   concat[dxname,R.get_op_conv(op_n),s2]
                        (* end case*))
                    in (name, alpha2@dx)
                    end
                | E.Probe(e1,e2)            => let
                    val (n1, beta1) = getAlpha (e1)
                    val (n2, _) = getAlpha (e2)
                    in  (concat[n1], beta1)
                        (*(concat["(",n1,")(", (n2),")"], beta1)*)
                    end
                | E.Const n=> if(n<0) then     (concat["-",Int.toString(~n)],[]) else  (Int.toString(n),[])
                | E.Lift (e1) => let
                    val (s1, alpha1) =getAlpha(e1)
                    val name1 = concat["Lift(",s1,")"]
                    in (name1, alpha1)  end
                | E.Op2(E.Div, E.Op1(E.Neg, e1),e2) =>  getAlpha (E.Op1(E.Neg, E.Op2(E.Div,  e1,  e2)))
                | E.Op1(E.PowInt n, e1) => let
                    val (s1, alpha1) =getAlpha(e1)
                    in (mkPow(op_n,s1, n), []) end
                (*flatten list *)
                | E.Op1(E.Sqrt, E.Op1(E.PowInt 2, e1)) => wrapNorm e1
                | E.Op1(E.Abs, e1) =>  wrapS("abs", e1)
                | E.Op1(E.Sqrt, inner as E.Sum(sx, E.Opn(E.Prod, [e1,e2]))) => let
                    fun reg()  = wrap(mkUnary(op_n,E.Sqrt),  inner)
                    in
                        if(EinUtil.sameExp(e1,e2))
                        then (case (sx, getAlpha(e1))
                            of ([(vid,_,_)], (s1, [E.V v0])) => if(vid=v0)  then wrapNorm e1 else reg()
                            | ([(vid1,_,_),(vid2,_,_)], (s1, [E.V v1,E.V v2])) => if(vid1=v1 andalso vid2=v2)  then wrapNorm e1 else reg()
                            | _ => reg()
                            (* end case*))
                        else  reg()
                    end
                    
                (*generic expression*)
                | E.Op1(E.Neg, e1) => let
                    val (s1,alpha1) = wrap(mkUnary(op_n,E.Neg), e1)
                    val _ = (String.concat["\n mkNegation:", s1, alphaToStr(alpha1)])
                   in  (s1,alpha1) end
                | E.Op1(op1, e1) => wrap(mkUnary(op_n,op1), e1)
                (*det *)
                | E.Op2(E.Sub,
                    e1 as E.Opn(E.Prod, [E.Tensor(ida, [E.C 0, E.C 0]), E.Tensor(idb, [E.C 1, E.C 1])]),
                    e2 as E.Opn(E.Prod, [E.Tensor(idc, [E.C 0, E.C 1]), E.Tensor(idd, [E.C 1, E.C 0])])) =>
                    let
            
                    in
                        if(ida = idb andalso ida=idc andalso ida=idd)
                        then (mkDet2(cvtId(args,ida)),[])
                        else  let
                            val (s1, alpha1) = getAlpha (e1)
                            val (s2, alpha2) = getAlpha (e2)
                            in checkSubt(e, s1, alpha1, s2, alpha2) end
                    end
                | E.Op2(E.Sub,
                    e1 as E.Opn(E.Prod, [E.Tensor(ida, [E.C 1, E.C 1]), E.Tensor(idb, [E.C 0, E.C 0])]),
                    e2 as E.Opn(E.Prod, [E.Tensor(idc, [E.C 1, E.C 0]), E.Tensor(idd, [E.C 0, E.C 1])])) =>
                    let
                    
                    in
                    if(ida = idb andalso ida=idc andalso ida=idd)
                    then (mkDet2(cvtId(args,ida)),[])
                    else  let
                    val (s1, alpha1) = getAlpha (e1)
                    val (s2, alpha2) = getAlpha (e2)
                    in checkSubt(e, s1, alpha1, s2, alpha2) end
                    end
                | E.Op2(E.Sub,
                    e1 as E.Opn(E.Prod, [E.Field(ida, [E.C 0, E.C 0]), E.Field(idb, [E.C 1, E.C 1])]),
                    e2 as E.Opn(E.Prod, [E.Field(idc, [E.C 0, E.C 1]), E.Field(idd, [E.C 1, E.C 0])])) =>
                    let
               
                    in
                    if(ida = idb andalso ida=idc andalso ida=idd)
                    then (mkDet2(cvtId(args,ida)),[])
                    else  let
                    val (s1, alpha1) = getAlpha (e1)
                    val (s2, alpha2) = getAlpha (e2)
                    in checkSubt(e, s1, alpha1, s2, alpha2) end
                    end
                | E.Op2(E.Sub,
                    e1 as E.Opn(E.Prod, [E.Field(ida, [E.C 1, E.C 1]), E.Field(idb, [E.C 0, E.C 0])]),
                    e2 as E.Opn(E.Prod, [E.Field(idc, [E.C 1, E.C 0]), E.Field(idd, [E.C 0, E.C 1])])) =>
                    let
                    
                    in
                        if(ida = idb andalso ida=idc andalso ida=idd)
                            then (mkDet2(cvtId(args,ida)),[])
                        else  let
                            val (s1, alpha1) = getAlpha (e1)
                            val (s2, alpha2) = getAlpha (e2)
                            in checkSubt(e, s1, alpha1, s2, alpha2) end
                    end
                | E.Op2(E.Div, e1, e2) =>   let
                            val (s1, alpha1) = getAlpha (e1)
                            val (s2, alpha2) = getAlpha (e2)
                            val _ = (String.concat["\n mkdivide-num:", s1, alphaToStr(alpha1)," denom:",s2,alphaToStr(alpha2)])
                        in ( mkDivide(op_n,s1,s2, flag_pulldiv), alpha1) end
                | E.Op2(op2, e1, e2) =>  let
         
                    val (s1, alpha1) = getAlpha (e1)
                    val (s2, alpha2) = getAlpha (e2)
                     val (name,beta) =  (case op2
                        of E.Sub => checkSubt(e, s1, alpha1, s2, alpha2)
                        | E.Div => let val _ = ((concat["\n\nDIV"," :$",s1,"\t:",s2,"$\n",EinPP.HeadexpToString(e1),"\n",EinPP.HeadexpToString(e2)])) in ( mkDivide(op_n,s1,s2, flag_pulldiv), alpha1) end
                        | E.Max => (concat["Max (",s1,",",s2,")"], [])
                        | E.Min => (concat["Min (",s1,",",s2,")"], [])
                        (* end case*))
                         in
                            (name,beta)
                    end
                | E.Opn(E.Add, es) =>let
                    fun sortAdd [] =[]
                      | sortAdd (E.Opn(E.Add, ps)::es) = sortAdd(ps@es)
                      | sortAdd(e1::es) = e1::sortAdd(es)
                   
                    fun mkAdd [] = E.Const 0
                      | mkAdd [e1] = e1
                      | mkAdd es = E.Opn(E.Add, sortAdd es)
                      
                    fun orgAdd es  = let
                        fun iter([], pos, []) = mkAdd pos
                        | iter([], [], neg) = E.Op1(E.Neg, mkAdd neg)
                        | iter([], pos, neg) =  E.Op2(E.Sub, mkAdd pos, mkAdd neg)
                        | iter(E.Op1(E.Neg, e1)::es, pos, neg) = iter(es, pos, e1::neg)
                        | iter(E.Op2(E.Div, E.Op1(E.Neg, e1), e2)::es, pos, neg) = iter(es, pos, E.Op2(E.Div, e1,e2)::neg)
                        | iter(e1::es, pos, neg) = iter(es, e1::pos, neg)
                        in iter(es,[], []) end
                    
    
                   in (case orgAdd es
                        of E.Opn(E.Add, es)=> getAdd es
                        | e  => getAlpha(e)
                        (*end case*))
                    end
                | E.Opn(E.Prod, es) => getProd es
                | E.Sum([(vid1, _,_)], E.Opn(E.Prod, [e1, E.Sum([(vid2, _,_)], E.Opn(E.Prod, [E.Eps2(i, j), e4]))])) =>
                
                    (* checks for cross product *)
                    let
        
                      val (s1, alpha1) = getAlpha (e1)
                        val (s4, alpha4) = getAlpha (e4)
                        val name = mkCross(op_n,s1, s4)
                    in
                        if (checkIndex(alpha1, i) andalso checkIndex(alpha4, j))
                        then (name, [])
                        else
                            if(checkIndex(alpha1, j) andalso checkIndex(alpha4, i))
                            then (name, []) else unhandled ("61",e)
                    end
                | E.Sum([(vid1, _,_)], E.Opn(E.Prod, [e1, E.Sum([(vid2, _,_)], E.Opn(E.Prod, [E.Epsilon(i, j, k), e4]))])) =>
                            (* checks for cross product *)
                            let
                            val (s1, alpha1) = getAlpha (e1)
                            val (s4, alpha4) = getAlpha (e4)
                            
                            val tmp1 = (concat["\n\tfirst A1:",alphaToStr(alpha1),s1,"\n\tA2:",alphaToStr(alpha4),s4,"\n\tsx:",Int.toString(vid1),"\tsx:",Int.toString(vid2), "\n\n"])
                            
      
                           
                            in if(checkEps(E.V vid1, E.V vid2, j, k, alpha1,alpha4))
                                then (mkCross(op_n,s1, s4),[i])
                                else let
                                    val (s1, alpha1) = mkOrdered(op_n,s1, alpha1)
                                    val (s4, alpha4) = mkOrdered(op_n,s4, alpha4)
                                    val tmp2 = (concat["\n\tsecond A1:",alphaToStr(alpha1),s1,"\n\tA2:",alphaToStr(alpha4),s4,"\n\tsx:",Int.toString(vid1),"\tsx:",Int.toString(vid2), EinPP.expToString(e), "\n\n"])
                                    in if(checkEps(E.V vid1, E.V vid2, j, k, alpha1,alpha4))
                                        then (mkCross(op_n,s1, s4),[i])
                                        else  unhandled ("z"^tmp2,e)
                                    end
                            end
                | E.Sum([(vid1, _,_)], E.Opn(E.Prod, [e1, E.Sum([(vid2, _,_)], E.Opn(E.Prod, [e4, E.Epsilon(i, j, k)]))])) =>
                            (* checks for cross product *)
                            let
                            val (s1, alpha1) = getAlpha (e1)
                            val (s4, alpha4) = getAlpha (e4)
                            in  if(checkEps(E.V vid1, E.V vid2, j, k, alpha1,alpha4))
                                then (mkCross(op_n,s1, s4),[i])
                                else unhandled ("63",e)
                            end
                | E.Sum([(vid1, _,_)], E.Opn(E.Prod, [E.Eps2(i, E.V j), e2])) =>
                    (*checks eps tensor term*)
                    let
                        val (s2, alpha2) = getAlpha (e2)
                    in (case alpha2
                        of [E.V v1]  =>
                            if(vid1=j andalso vid1=v1)
                            then (mkInner(op_n,"EPS(2)",s2),[i])
                            else unhandled ("34",e)
                        | _ =>  unhandled ("35",e)
                        (*end case *))
                    end
                | E.Sum([(vid1, _,_)], E.Opn(E.Prod, [E.Epsilon(i, j,E.V k), e2])) =>
                    (*checks eps tensor term*)
                    let
                        val (s2, alpha2) = getAlpha (e2)
                     in (case alpha2
                        of (E.V v1::vs)  =>
                                if(vid1=k andalso vid1=v1)
                                then (mkInner(op_n,"EPS(3)",s2),[i,j])
                                else unhandled ("39",e)
                        | _ =>  unhandled ("35",e)
                        (*end case *))
                    end
                  
                | E.Sum([(vid1, _,_)], E.Opn(E.Prod, [e1, inner])) =>
                    (* checks for inner product *)
                        let
                        val (s1, alpha1) = getAlpha (e1)
                        val (s2, alpha2) = getAlpha (inner)
                        val n1 =length(alpha1)
                        val n2 =length(alpha2)
                        (* are the summation indices in each term *)
                        val p1 = checkIndices(alpha1, vid1)
                        val p2 = checkIndices(alpha2, vid1)
                        val  _ = (concat["\n\ne:", EinPP.expToString(e1),"\n\ninner:", EinPP.expToString(inner) ])
                        val tmp1 = (concat["\n\tA1:",alphaToStr(alpha1),"\n\tA2:",alphaToStr(alpha2),"\n\tsx:",Int.toString(vid1),"\n\n"])
                        in (case (p1,p2)
                            of (NONE, NONE) => unhandled("91", e)
                            | (SOME _, NONE) => unhandled("92", e)
                            | (NONE, SOME _) => unhandled("93", e)
                            | (SOME (1, [v1]),  SOME(1,[])) => let
                                    (*M_ji V_j*) (*switch ordering*)
                                val name = mkInner(op_n,s2,s1)
                                in (name, [v1]) end
                            | (SOME (1, [v1,v2]),  SOME(1,[])) => let
                                (*M_kab V_k*) (*switch ordering*)
                                val name = mkInner(op_n,s2,s1)
                                val tmp = (concat["\n\ta-A1:",alphaToStr(alpha1), " spot1:", Int.toString(2),
                                " beta1:",alphaToStr([v1,v2]),"\n\tA2:",alphaToStr(alpha2)," spot2:",Int.toString(1)," beta2:",alphaToStr([]),"\n\tsx:",Int.toString(vid1),"\n\n"])
                                         val _ = ("\nInner:"^name)
                                in (name, [v1,v2]) end
                                
                            | (SOME (spot1, beta1),  SOME(spot2, beta2)) =>
                                (*sum indices are in there*)
                                let
                                      val _ = "\n\n\n____"
                                val _ = (EinPP.expToString(e1))
                                val _ = "--"
                                                     val _ = (EinPP.expToString(inner))
                                val tmp = (concat["\n\tb-A1:",alphaToStr(alpha1), " spot1:", Int.toString(spot1),
                                " beta1:",alphaToStr(beta1),"\n\tA2:",alphaToStr(alpha2)," spot2:",Int.toString(spot2)," beta2:",alphaToStr(beta2),"\n\tsx:",Int.toString(vid1),"\n\n"])
                                val _ =  tmp
                                val s3= if(spot1=n1) then s1
                                        else (case (alpha1,spot1)
                                            (*spot should be last but is somewhere else*)
                                            of ([_,_],1) => mkTranspose(op_n,s1)
                                            | ([_ , E.V v0, E.V v1], 1) =>
                                                mkTraversedI(s1, 2::sort ([v0,v1]))
                                            | ([E.V v0,_,E.V v1], 2)   =>
                                                let
                                                val [a,b] = sort ([v0,v1])
                                                in  mkTraversedI(s1, [a,2,b]) end
                                            | ([_ , E.V v0, E.V v1, E.V v2], 1) =>
                                                mkTraversedI(s1, 3::sort ([v0,v1,v2]))
                                            | ([E.V v0, _, E.V v1, E.V v2], 2) =>
                                                let
                                                val [a,b,c] = sort ([v0,v1,v2])
                                                in  mkTraversedI(s1, [a,3,b,c]) end
                                            | ([E.V v0, E.V v1, _, E.V v2], 3) =>
                                                let
                                                val [a,b,c] = sort ([v0,v1,v2])
                                                in  mkTraversedI(s1, [a,b,3, c]) end
                                                
                                            | _ => let val (s1,alpha)=  unhandled("25", e) in  s1 end
                                            (*end case*))
                                val s4 = if(spot2=1) then s2
                                        else (case (alpha2,spot2)
                                            (*spot should be first but is somewhere else*)
                                            of ([_, _], 2)      =>  mkTranspose(op_n,s2)
                                            | ([E.V v0,_,E.V v1], 2)   =>
                                                let
                                                val [a,b] = sort ([v0,v1])
                                                in  mkTraversedI(s2, [a+1,0,b+1]) end
                                            | ([E.V v0, E.V v1, _], 3) =>let
                                                val [a,b] = sort ([v0,v1])
                                                val _ =  (concat["\nhere",Int.toString(a),"-",Int.toString(b)])
                                                in mkTraversedI(s2,  ([a+1,b+1])@[0]) end
                                            
                                            | ([E.V v0, _, E.V v1, E.V v2], 2) =>
                                                let
                                                val [a,b,c] = sort ([v0,v1,v2])
                                                in  mkTraversedI(s2, [a+1,0,b+1,c+1]) end
                                            | ([E.V v0, E.V v1, _, E.V v2], 3) =>
                                                let
                                                val [a,b,c] = sort([v0,v1,v2])
                                                in  mkTraversedI(s2, [a+1,b+1,0, c+1]) end
                                            | ([E.V v0, E.V v1, E.V v2, _], 4) =>
                                                let
                                                val [a,b,c] = sort([v0,v1,v2])
                                                in  mkTraversedI(s2, [a+1,b+1, c+1, 0]) end
                                            | _ => let val (s2,alpha)=  unhandled("27", e) in  s2 end
                                            (*end case*))
                                val name = mkInner(op_n,s3,s4)
                                val _ = ("\nInner:"^name)
                                val beta = beta1@beta2
                                in  (name, beta)
                                end
                            (* end case*))
                            end
                    
                | E.Sum([(vid1, _,_)], e1) => let
                     val (s1, alpha1) = getAlpha (e1)
                     in (case alpha1
                     of ((E.V a)::(E.V b)::es) =>
                            if(a=b andalso vid1=a) then (mkTrace(s1),es)
                            else unhandled (concat["82 vid1:",Int.toString(vid1),"\n\tstarting A1:", alphaToStr(alpha1),"----",s1,"\n\n\t",EinPP.HeadexpToString(e1) ],e)
                        | [E.V a] =>
                            if(vid1=a) then (mkReduce(s1),[])
                            else unhandled (concat["83 vid1:",Int.toString(vid1),"\n\tstarting A1:",s1, alphaToStr(alpha1)],e)
                        | _ => unhandled ("81",e)
                        (* end case*))
                     end
                | E.Sum([(vid1, _,_),(vid2, _,_)], E.Opn(E.Prod, [E.Epsilon(i,j,k), e2, e3])) =>
                    let
                        (* checks for cross product *)
                        val (s2, alpha2) = getAlpha (e2)
                        val (s3, alpha3) = getAlpha (e3)
                        in
                            if(checkEps(E.V vid1, E.V vid2, j, k, alpha2,alpha3))
                            then  (mkCross(op_n,s2, s3), [i])
                            else unhandled ("7",e)
                    end
                | E.Sum([(vid1a, _,_),(vid2b, _,_)], E.Opn(E.Prod, [e1,e2])) =>
                    let
                        val (vid1, vid2) = if(vid1a<vid2b) then (vid1a, vid2b) else (vid2b, vid1a)
                        val (s1, alpha1) = getAlpha (e1)
                        val (s2, alpha2) = getAlpha (e2)

                         val tmp1 = (concat["\n\tDOUBLE starting A1:",s1, "\tindex1:", alphaToStr(alpha1),"\n\t:",EinPP.expToString(e1),"\n\tA2:",s2, "\tindex2:", alphaToStr(alpha2),"\n\t:",EinPP.expToString(e2),"\n\tsx:",Int.toString(vid1),"-",Int.toString(vid2),"\n\n"])
                         
                         
                        (* are the summation indices in each term *)
                        val (s1, beta1) = (case  checkIndices(alpha1, vid1)
                            of SOME (spot1, beta1) =>
                                (case checkIndices(alpha1, vid2)
                                    of SOME (spot2, beta1) =>
                                        (case (spot1, spot2, alpha1)
                                        of (1, 2, [_,_]) => let
                                            val _ = (concat["\nspot",Int.toString(spot1),"-",Int.toString(spot2)])
                                            in (s1,[]) end
                                        |  (2, 1, [_,_]) => (mkTranspose(op_n,s1), [])
                                        |  (1, 2, [_,_,c]) =>
                                            (mkTraversedS(s1, "1,2,0"), [c])
                                        |  (2, 1, [_,_,c]) =>
                                            (mkTraversedS(s1, "2,1,0"), [c])
                                        | _ => unhandled ("71 spot1: "^Int.toString(spot1)^" spot2: "^Int.toString(spot2),e)
                        
                                        (*end case*))
                                    | NONE =>  unhandled ("72",e)
                                 (*end case*))
                            | NONE =>  unhandled ("73",e)
                            (*end case*))
                            
                        val tmp1 = (concat["\n\tDOUBLE  iter 2 A1:",s1, alphaToStr(alpha1),"\n\tA2:",s2, alphaToStr(alpha2),"\n\tsx:",Int.toString(vid1),"-",Int.toString(vid2),"\n\n"])
                         
                         
                         
                        (* are the summation indices in each term *)
                        val (s2, beta2) = (case  checkIndices(alpha2, vid1)
                            of SOME (spot1, beta2) =>
                                (case checkIndices(alpha2, vid2)
                                    of SOME (spot2, beta2) =>
                                        (case (spot1, spot2, alpha2)
                                            of (1, 2, (_::_::beta2)) => (s2, beta2)
                                            |  (2, 1, [_,_])   => (mkTranspose(op_n,s2), [])
                                            |  (2, 1, (_::_::beta2)) => let
                                                val nbeta = length (beta2)
                                                val colns = List.tabulate(nbeta, fn n=> ",:")
                                                in (mkTraversedS(s2,  concat ("1,0"::colns)), beta2) end
                                            |  (2, 3, (a::b::c::beta2)) => let
                                                val nbeta = length (beta2)
                                                val colns = List.tabulate(nbeta, fn n=> ",:")
                                                in (mkTraversedS(s2,  concat ("2,0,1"::colns)), a::beta2) end
                                                
                                            | _ => unhandled (concat["74 spot1: ",Int.toString(spot1)," spot2:",Int.toString(spot2)],e)
                                        (*end case*))
                                    | NONE =>  unhandled ("75",e)
                                    (*end case*))
                            | NONE =>  unhandled ("76",e)
                            (*end case*))
                        
                            val tmp1 = (concat["\n\tDOUBLE  iter 3 A1:",s1, alphaToStr(alpha1),"\n\tA2:",s2, alphaToStr(alpha2),"\n\tsx:",Int.toString(vid1),"-",Int.toString(vid2),"\n\n"])
                                     
                        val beta = beta1@beta2
                        
                        val tmp1 = (concat["\n\tDOUBLE  beta1", alphaToStr(beta1),"\n\tbeta2:", alphaToStr(beta2),"\n\treg beta:",alphaToStr(beta),"\n\n"])
                           
                        in  (mkDouble(op_n,s1,s2), beta) end
                | E.Sum([(vid1, _,_),(vid2, _,_)], E.Opn(E.Prod, [_,_,_])) => unhandled ("33",e)
                | E.Sum([(vid1, _,_),(vid2, _,_)], E.Opn(E.Prod, [_,_,_,_])) => unhandled ("34",e)
                | E.Sum(_, _) => unhandled ("31",e)
                | E.Opn(_, _) => unhandled ("32",e)
                | E.If(comp, e2, e3) => let
                    val (s2, alpha2) = getAlpha (e2)
                    val (s3, alpha3) = getAlpha (e3)
                    val (cop, e4, e5) = (case comp
                        of E.GT(e4,e5) =>   (">",e4,e5)
                        | E.LT(e4,e5)  =>   ("<",e4,e5)
                    (* end case *))
                    val (s4, _) = getAlpha (e4)
                    val (s5, _) = getAlpha (e5)
                    val cname = concat["(",s4,cop,s5,")"]
                    val name =  concat(["\nIf ",cname,"\n\t then ", s2, "\n\t else ",s3])
                    in (name, alpha2) end
                | _ => unhandled ("8",e)
                (*end case*))
            val (name, beta) =  if(!complex) then ("?",[E.V ~1]) else run(e)
        
            val _ = (concat["\n\n----",name, "---",alphaToStr(beta)," ---",EinPP.expToString(e)])
            in (case beta
                of [E.C c1] => (mkSlice(name, Int.toString(c1)),[])
                | [E.C c1, E.C c2] => (mkSlice(name, concat[Int.toString(c1),",",Int.toString(c2)]),[])
                | [E.C c1, E.V v1] => (mkSlice(name, Int.toString(c1)^",:"),[E.V v1])
                | [E.V v1, E.C c1] => (mkSlice(name, ":,"^Int.toString(c1)),[E.V v1])
                | [a as E.C c1, b, c] => let
                    fun format([], inner, shape) = (inner, shape)
                    | format (E.C c2::es, inner, shape) = format(es, inner@[Int.toString(c2)], shape)
                    | format (E.V v2::es, inner, shape) = format(es, inner@[":"], shape@[E.V v2])
                    val (inner, shape) = format([a,b,c], [],[])
                    val terms = String.concatWith "," inner
                    in (mkSlice(name, terms),shape) end
                | [a as E.C c1, b, c, d] => let
                    fun format([], inner, shape) = (inner, shape)
                    | format (E.C c2::es, inner, shape) = format(es, inner@[Int.toString(c2)], shape)
                    | format (E.V v2::es, inner, shape) = format(es, inner@[":"], shape@[E.V v2])
                      
                    val (inner, shape) = format([a,b,c,d], [],[])
                    val terms = String.concatWith "," inner
                    
                    in (mkSlice(name, terms),shape) end
                | _ => (name, beta)
                (**))
            end
    (*scan ein *)
    val (s, alpha) = getAlpha body
    in if(!complex) then (!errterm,[E.V ~1]) else  (s, alpha)
    end
    (* ------------------------------------------------------------------ *)
    (* ------------------------------------------------------------------ *)
    fun scanEin(op_n, args,lhs, e as Ein.EIN{params, index, body}, flag_pulldiv,flag_shiftAdd) =
        let

            val shift = 0 (*expected index shift *)
            val (name, beta) = scan(op_n,args,body, flag_pulldiv,flag_shiftAdd)
        in

            if (isOrdered(beta, shift))
            then name
            else  if (isOrdered(List.rev beta, shift))
                    then concat["(",name,")^T"]
                    else "----No surface syntax----"
        end



end
