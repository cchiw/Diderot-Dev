(* ein-pp.sml
 *
 * NOTE: this code probably should be merged into the EinUtil module.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure EinPP : sig

    val toString : Ein.ein -> string

    val expToString : Ein.ein_exp -> string

  end = struct

    structure E = Ein

    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s

    fun index2s (E.C cx) = concat["'", i2s cx, "'"]
      | index2s (E.V ix) = "i" ^ i2s ix

    fun multiIndex2s [] = ""
      | multiIndex2s alpha = concat ["_{", String.concatWithMap "," index2s alpha, "}"]

    fun delta (a, b) = concat["δ_{", index2s a, ",", index2s b,"}"]
    fun deltaKrn (a, b) = concat["δ_{", index2s a, ",", index2s b,"}"]
    fun border E.Default = "Default"
      | border E.Clamp = "Clamp"
      | border E.Mirror =" Mirror"
      | border E.Wrap = "Wrap"
      | border E.None = raise Fail "unexpected"
    fun deriv [] = ""
      | deriv alpha = concat["∇",multiIndex2s alpha]
    fun expToString e = (case e
           of E.Const r => i2s r
            | E.ConstR r => Rational.toString r
            | E.Tensor(id, []) => "T" ^ i2s id
            | E.Tensor(id, alpha) => concat["T", i2s id, multiIndex2s alpha]
            | E.Zero(alpha) => concat["Z", multiIndex2s alpha]
            | E.Delta ix => delta ix
            | E.Epsilon(ix, jx, kx) => concat["ϵ_{i", index2s ix, ",i", index2s jx, ",i", index2s kx, "}"]
            | E.Eps2(ix, jx) => concat["ϵ_{i", index2s ix, ",i", index2s jx, "}"]
            | E.Field(id, []) => "F" ^ i2s id
            | E.Field(id, alpha) => concat["F", i2s id, multiIndex2s alpha]
            | E.Lift e1 => concat["«", expToString e1, "»"]
            | E.Conv(img, alpha, kern, beta) => let
                val alpha = if null alpha then "" else multiIndex2s alpha
                val beta = if null beta then "" else "dx" ^ multiIndex2s beta
                in
                 (* concat ["V", i2s img, alpha, "⊛", beta, "H", i2s kern]*)
                   concat ["F", i2s img, alpha, "_", beta]
                end
            | E.Partial alpha => "∂/∂x" ^ multiIndex2s alpha
            | E.Apply(e1, e2) => concat [ expToString e1, "@(", expToString e2, ")"]
            | E.Probe(e1, e2) => concat ["Probe(", expToString e1, ",", expToString e2, ")"]
            | E.Comp(e1,es) => let
                fun iter ([]) = ""
                | iter ((e2, n1)::es) =
                concat ["[", expToString e2 , concat ["{", shp2s n1, "}"],  "]", iter(es)]
                in concat ["Cmp(", expToString e1,")", (iter(es))] end
            | E.OField(E.PolyWrap(es), e1, [])
                => concat ["PolyWrap[", String.concatWithMap " ," expToString  es,  "](",expToString e1,")"]
            | E.OField(E.PolyWrap(es), e1, alpha)
                => concat [  expToString(E.OField(E.PolyWrap(es), e1, [])) ,"dx",multiIndex2s alpha, ")"]
 
            (* | E.OField(E.PolyWrap _, e1, []) => concat ["PolyWrap(", expToString e1  ,")"]
            | E.OField(E.PolyWrap _, e1, alpha) => concat ["PolyWrap(", expToString e1, "dx^", multiIndex2s alpha, ")"]
            *)
            | E.OField(E.DataFem id, e1, alpha) => concat ["DataFEM(",expToString e1,")_",i2s id, deriv alpha, ")"]
            | E.OField(E.BuildFem (id,id2), e1, alpha) => concat ["BuildFEM(",expToString e1,")_", i2s id, "[",i2s id2,"]",deriv alpha, ")"]
            | E.OField(E.ManyPointerBuildFem(id,id2, id3, id4), e1, alpha) => concat ["ManyPtr(",expToString e1,")_", i2s id, "[",i2s id2,"|",i2s id3,"|",i2s id4,"]",deriv alpha, ")"]
            (* | E.Poly(tid, cx, 1, []) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")"]*)
            | E.Poly(tid, cx, 1, dx) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")",deriv dx]
            | E.Poly(tid, cx, n, dx) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")^",  i2s n, deriv  dx]
            | E.Value ix => "i" ^ i2s ix
            | E.Img(fid, alpha, pos, s, E.None) => concat [
                  "V", i2s fid, multiIndex2s alpha, "(", i2s s, ")[",
                  String.concatWithMap "," expToString pos, "]"
                ]
            | E.Img(fid, alpha, pos, s, b) => concat [
                  "V", i2s fid, multiIndex2s alpha, "(", i2s s, ")[",
                  String.concatWithMap "," expToString pos, "]#", border b
                ]
            | E.Krn(tid, [], dim) => concat["H", i2s tid, "(", Int.toString dim, ")"]
            | E.Krn(tid, betas, dim) => concat[
                  "H", i2s tid, "^{", String.concatWithMap "" deltaKrn betas, "}(", Int.toString dim, ")"
                ]
             | E.Sum(sx, e) => let
                val sx = List.map
                      (fn (v, lb, ub) => concat ["(i", i2s v, (*"=", i2s lb, "..", i2s ub, *)")"])
                        sx
                in
                  concat ("ΣΣ" :: sx @ ["<(", expToString e, ")>Σ"]@sx)
                end
            | E.Op1(E.PowInt n, e) => concat["(", expToString e , ")^", i2s n]
            | E.Op1(f, e) => let
                val f = (case f
                       of E.Neg => "Neg"
                        | E.Exp => "Exp"
                        | E.Sqrt => "Sqrt"
                        | E.Cosine => "Cosine"
                        | E.ArcCosine => "ArcCosine"
                        | E.Sine => "Sine"
                        | E.ArcSine => "ArcSine"
                        | E.Tangent => "Tangent"
                        | E.ArcTangent => "ArcTangent"
                        | E.Abs => "Absolute"
                        | _ => raise Fail "impossible"
                      (* end case *))
                in
                  concat[f, "(", expToString e, ")"]
                end
           | E.Op2(E.Max, e1, e2) => concat ["Max(", expToString e1, ") , (", expToString e2, ")"]
           | E.Op2(E.Min, e1, e2) => concat ["Min(", expToString e1, ") , ( ", expToString e2, ")"]
           | E.Op2(E.Sub, e1, e2) => concat ["(", expToString e1, ") - (", expToString e2, ")"]
           | E.Op2(E.Div, e1, e2) => concat ["(", expToString e1, ") / ( ", expToString e2, ")"]
           | E.Opn(E.Add, el) => concat["(", String.concatWithMap " + " expToString el,")"]
           | E.Opn(E.Prod, el) => concat["(", String.concatWithMap " * " expToString el, ")"]
           | E.Opn(E.Swap (id), es) => concat["SWAP[",i2s id,"](", String.concatWithMap ", " expToString es,")"]
           | E.BigF  (_,id, alpha) => concat [deriv alpha, "ϝ"(*,i2s id*)]
           | E.Basis (_,id, alpha) => concat [deriv alpha,"ϕ"(*,i2s id,*)]
           | E.Inverse (_,e1) => concat ["(", expToString e1, ")","¯¹"]
           | E.EvalFem (_,[e1,e2]) => concat [expToString e1,"[",expToString e2,"]" ]
           | E.EvalFem (_, [e1,e2,e3]) =>  concat [expToString e1,"[",expToString e2,"[",expToString e3,"]","]" ]
           | E.If (comp, e3, e4) => let
                val c = (case comp
                    of E.GT(e1, e2) => concat [expToString e1, ">", expToString e2]
                    | E.LT(e1, e2)  => concat [expToString e1, "<", expToString e2]
                        (*end case*))
               in
                concat[ "\nif(", c, ") then ", expToString e3," else ", expToString e4]
               end
          (* end case *))

    fun toString (Ein.EIN{params, index, body}) = let
          fun paramToString (i, E.TEN(t, shp)) = concat["T", i2s i, "[", shp2s shp, "]"]
            | paramToString (i, E.FLD (d,shp)) = concat["F", i2s i, "[", shp2s shp, "]"]
            | paramToString (i, E.KRN) = "H" ^ i2s i
            | paramToString (i, E.IMG(d, shp)) = concat["V", i2s i, "(", i2s d, ")[", shp2s shp, "]"]
            | paramToString (i, E.DATA) = "DATA" ^ i2s i
            | paramToString (i, E.FNCSPACE) = "FNCSPACE" ^ i2s i
          val params = String.concatWith "," (List.mapi paramToString params)
          val index = if null index then "" else concat["_{", shp2s index, "}"]
          in
            concat["λ(", params, ")<", expToString body, ">", index]
          end

    fun HeadexpToString e = (case e
        of E.Const r => i2s r
        | E.ConstR r => Rational.toString r
        | E.Tensor(id, alpha) => concat["T", i2s id]
        | E.Zero(alpha) => concat["Z", multiIndex2s alpha]
        | E.Delta ix => delta ix
        | E.Epsilon(ix, jx, kx) => concat["ϵ_{i", index2s ix, ",i", index2s jx, ",i", index2s kx, "}"]
        | E.Eps2(ix, jx) => concat["ϵ_{i", index2s ix, ",i", index2s jx, "}"]
        | E.Field(id, []) => "f" ^ i2s id
        | E.Field(id, alpha) => concat["f", i2s id]
        | E.Lift e1 => concat["«-»"]
        | E.Conv(img, alpha, kern, beta) => let
            val alpha = if null alpha then "" else multiIndex2s alpha
            val beta = if null beta then "" else "dx" ^ multiIndex2s beta
            in
                concat ["V", i2s img, alpha, "⊛", beta, "H", i2s kern]
            end
        | E.Partial alpha => "∂/∂x" ^ multiIndex2s alpha
        | E.Apply(e1, e2) => "∂/∂x"
        | E.Probe(e1, e2) => concat ["Probe"]
        | E.Comp(e1,es) => concat ["Cmp"]
        | E.OField(E.PolyWrap _, e1, []) => concat ["PolyWrap"]
        | E.OField(E.PolyWrap _, e1, alpha)  => concat ["PolyWrap"]
        | E.Poly(tid, cx, 1, []) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")"]
        | E.Poly(tid, cx, 1, dx) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ") dx",multiIndex2s  dx]
        | E.Poly(tid, cx, n, []) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")^",  i2s n]
        | E.Poly(tid, cx, n, dx) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")^",  i2s n, " dx", multiIndex2s  dx]
        | E.Value ix => "i" ^ i2s ix
        | E.Img(fid, alpha, pos, s, E.None) => concat [
            "V", i2s fid, multiIndex2s alpha, "(", i2s s, ")[-]"
            ]
        | E.Img(fid, alpha, pos, s, b) => concat [
            "V", i2s fid, multiIndex2s alpha, "(", i2s s, ")[-]#", border b
            ]
        | E.Krn(tid, [], dim) => concat["H", i2s tid, "(", Int.toString dim, ")"]
        | E.Krn(tid, betas, dim) => concat[
            "H", i2s tid, "^{", String.concatWithMap "" deltaKrn betas, "}(", Int.toString dim, ")"
            ]
        | E.Sum(sx, e) => let
            val sx = List.map
                (fn (v, lb, ub) => concat ["(i", i2s v, ")"])
                sx
                in
                concat ("ΣΣ" :: sx)
                end
        | E.Op1(E.PowInt n, e) => concat["^", i2s n]
        | E.Op1(f, e) => let
            val f = (case f
                of E.Neg => "Neg"
                | E.Exp => "Exp"
                | E.Sqrt => "Sqrt"
                | E.Cosine => "Cosine"
                | E.ArcCosine => "ArcCosine"
                | E.Sine => "Sine"
                | E.ArcSine => "ArcSine"
                | E.Tangent => "Tangent"
                | E.ArcTangent => "ArcTangent"
                | E.Abs => "Absolute"
                | E.Sgn => "Sgn"
                | _ => raise Fail "impossible"
                (* end case *))
            in
                concat[f]
            end
        | E.Op2(E.Sub, e1, e2) => concat ["Subtract"]
        | E.Op2(E.Div, e1, e2) => concat ["Division"]
        | E.Op2(E.Max, e1, e2) => concat ["Max"]
        | E.Op2(E.Min, e1, e2) => concat ["Min"]
        | E.Opn(E.Add, el) => concat["Add"]
        | E.Opn(E.Prod, el) => concat["Prod"]
        | E.BigF  (id, alpha) => concat [deriv alpha, "ϝ"(*,i2s id*)]
        | E.Basis (id, alpha) => concat [deriv alpha,"ϕ"(*,i2s id,*)]
        | E.Inverse (e) => concat ["(", expToString e, ")","¯¹"]
        | E.EvalFem [e1,e2] => concat [expToString e1,"[",expToString e2,"]" ]
        | E.EvalFem [e1,e2,e3] =>  concat [expToString e1,"[",expToString e2,"[",expToString e3,"]","]" ]
        | E.If (comp, e3, e4) => let
            val c = (case comp
            of E.GT(e1, e2) => ">"
            | E.LT(e1, e2)  => "<"
            (*end case*))
            in
                concat[ "\nif(", c, ") then ", expToString e3," else ", expToString e4]
            end
        | E.Opn(E.Swap (id), es) => concat["SWAP[",i2s id,"](-)"]
    (* end case *))
  end
