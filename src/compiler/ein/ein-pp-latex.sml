structure EinPPLat : sig

    val toString_reader : Ein.ein -> string

  end = struct

    structure E = Ein

    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s

    fun index2s (E.C cx) = concat["'", i2s cx, "'"]
      | index2s (E.V ix) = "i" ^ i2s ix

    fun multiIndex2s [] = ""
      | multiIndex2s alpha = concat ["_{", String.concatWithMap "," index2s alpha, "}"]

    fun delta (a, b) = concat["\\delta_{", index2s a, ",", index2s b,"}"]
    fun deltaKrn (a, b) = concat["\\delta_{", index2s a, ",", index2s b,"}"]
    fun deriv [] = ""
      | deriv alpha = concat["\\nabla",multiIndex2s alpha]
    fun expToString e = (case e
           of E.Const r => i2s r
            | E.ConstR r => Rational.toString r
            | E.Tensor(id, []) => concat["T^{P", i2s id,"}"]
            | E.Tensor(id, alpha) => concat["T^{P", i2s id,"}", multiIndex2s alpha]
            | E.Zero(alpha) => concat["Z", multiIndex2s alpha]
            | E.Delta ix => delta ix
            | E.Epsilon(ix, jx, kx) => concat["\\mathcal{E}_{i", index2s ix, ",i", index2s jx, ",i", index2s kx, "}"]
            | E.Eps2(ix, jx) => concat["\\mathcal{E}_{i", index2s ix, ",i", index2s jx, "}"]
            | E.Field(id, []) => concat["f^{P", i2s id,"}"]
            | E.Field(id, alpha) => concat["f^{P", i2s id, "}",multiIndex2s alpha]
            | E.Lift e1 => concat["«", expToString e1, "»"]
            | E.Conv(img, alpha, kern, []) => let
                val alpha = if null alpha then "" else multiIndex2s alpha
                in
                    concat ["F^{P", i2s img, "}",alpha]
                end

            | E.Conv(img, alpha, kern, beta) => let
                val alpha = if null alpha then "" else multiIndex2s alpha
                val beta = if null beta then "" else "\\nabla" ^ multiIndex2s beta
                in
                   concat [beta, "F^{P", i2s img, "}",alpha]
                end
            | E.Partial alpha => "\nabla" ^ multiIndex2s alpha
            | E.Apply(e1, e2) => concat [ expToString e1, "@(", expToString e2, ")"]
            | E.Probe(e1, e2) =>  expToString e1
            | E.Comp(e1,es) => let
                fun iter ([]) = ""
                | iter ((e2, n1)::es) =
                concat ["[", expToString e2 , concat ["{", shp2s n1, "}"],  "]", iter(es)]
                in concat ["Cmp(", expToString e1,")", (iter(es))] end
            | E.OField(E.PolyWrap(es), e1, [])
                => concat ["CFExp[ids:", String.concatWithMap " ," i2s  es,  "](exp:",expToString e1,")"]
            | E.OField(E.PolyWrap(es), e1, alpha)
                => concat [  expToString(E.OField(E.PolyWrap(es), e1, [])) ,"dx",multiIndex2s alpha, ")"]
            | E.OField(E.DataFem id, e1, alpha) => concat ["DataFEM(",expToString e1,")_",i2s id, deriv alpha, ")"]
            | E.OField(E.BuildFem (id,id2), e1, alpha) => concat ["BuildFEM(",expToString e1,")_", i2s id, "[",i2s id2,"]",deriv alpha, ")"]
            | E.OField(E.ManyPointerBuildFem(id,id2, id3, id4), e1, alpha) => concat ["ManyPtr(",expToString e1,")_", i2s id, "[",i2s id2,"|",i2s id3,"|",i2s id4,"]",deriv alpha, ")"]
            | E.Poly(tid, cx, 1, dx) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")",deriv dx]
            | E.Poly(tid, cx, n, dx) => concat ["(P", i2s tid,"_", multiIndex2s  cx, ")^",  i2s n, deriv  dx]
            | E.Value ix => "i" ^ i2s ix
            | E.Img(fid, alpha, pos, s) => concat [
                  "V", i2s fid, multiIndex2s alpha, "(", i2s s, ")[",
                  String.concatWithMap "," expToString pos, "]"
                ]
            | E.Krn(tid, [], dim) => concat["H", i2s tid, "(", Int.toString dim, ")"]
            | E.Krn(tid, betas, dim) => concat[
                  "H", i2s tid, "^{", String.concatWithMap "" deltaKrn betas, "}(", Int.toString dim, ")"
                ]
             | E.Sum(sx, e) => let
                val sx = List.map
                      (fn (v, lb, ub) => i2s v)sx
                in
                    concat ("\\sum_{" :: sx @ ["}(", expToString e, ")"]@sx)
                end
            | E.Op1(E.Sqrt, e) => concat["\\sqrt{", expToString e, "}"]
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
           | E.Op2(E.Max, e1, e2) => concat ["Max(", expToString e1, " , ", expToString e2, ")"]
           | E.Op2(E.Min, e1, e2) => concat ["Min(", expToString e1, " ,  ", expToString e2, ")"]
           | E.Op2(E.Sub, e1, e2) => concat ["(", expToString e1, ") - (", expToString e2, ")"]
| E.Op2(E.Div, e1, e2) => concat ["\\frac{", expToString e1, "}{", expToString e2, "}"]
           | E.Op3(E.Clamp, e1, e2, e3) => concat["Clamp (", expToString e1, ",", expToString e2, ",", expToString e3,")"]
           | E.Opn(E.Add, el) => concat["(", String.concatWithMap " + " expToString el,")"]
           | E.Opn(E.Prod, el) => concat["(", String.concatWithMap " * " expToString el, ")"]
           | E.Opn(E.Swap (id), es) => concat["Swap[",i2s id,"](", String.concatWithMap ", " expToString es,")"]
           | E.If (comp, e3, e4) => let
                val c = (case comp
                    of E.GT(e1, e2) => concat [expToString e1, ">", expToString e2]
                    | E.LT(e1, e2)  => concat [expToString e1, "<", expToString e2]
                        (*end case*))
               in
                concat[ "\nif(", c, ") then ", expToString e3," else ", expToString e4]
               end
               
          (* end case *))



    fun toString_reader (Ein.EIN{params, index, body}) = let
        val params = EinPP.paramsToString (params)
        val index = if null index then "" else concat["{", shp2s index, "}"]
          in
          concat["Params:$\\lambda$", params, " \\\\ \\phantom{++}Body:$", expToString body, "$\\\\ \\phantom{++}Size:$", index,"$"]
        end

    end
