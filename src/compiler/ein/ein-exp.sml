(* ein-pp.sml
 *
 * NOTE: this code probably should be merged into the EinUtil module.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure EinExp : sig

    val toString : Ein.ein -> string



  end = struct

    structure E = Ein

     val i2s = Int.toString

    fun inOrder([], n) = true
    | inOrder ((E.V v1)::es, n) = if(v1=n) then inOrder(es, n+1) else false
    | inOrder (_,n) = false




    fun expandInner (body, index, args) = (case body
        of E.Sum(
            [(v, _, ub)],
            E.Opn(E.Prod, [E.Tensor(id1, alpha as _::_), E.Tensor(id2, beta as _::_)])
            ) => (case (matchFindLast(alpha, v), matchFindLast(beta, v))
            of ((SOME ix1, NONE), (SOME ix2, NONE)) => let
            (* v is the last index of alpha, beta and nowhere else *)
            val vecIX= ub+1
            val vecA = createP (args, vecIX, id1, ix1)
            val vecB = createP (args, vecIX, id2, ix2)
            in
            unroll (
            index, index,
            fn (avail, mapp) => ToVec.dotV (avail, mapp, vecA, vecB))
            end
            | _ => scalarExpand (params, body, index, args)
            (* end case *))
            | E.Sum(
            [(v1, lb1, ub1), (v2, lb2, ub2)],
            E.Opn(E.Prod, [E.Tensor(id1, alpha as _::_), E.Tensor(id2, beta as _::_)])
            ) => let
            fun check (v, ub, i, lb', ub') = (
            case (matchFindLast(alpha, v), matchFindLast(beta, v))
            of ((SOME ix1, NONE), (SOME ix2, NONE)) => let
            (* v is the last index of alpha, beta and nowhere else *)
            val vecIX = ub+1
            val vecA = createP (args, vecIX, id1, ix1)
            val vecB = createP (args, vecIX, id2, ix2)
            in
            SOME(unroll (
            index, index,
            fn (avail, mapp) =>
            ToVec.sumDotV (avail, mapp, i, lb', ub', vecA, vecB)))
            end
            | _ => NONE
            (* end case *))
            in
            case check(v1, ub1, v2, lb2, ub2)
            of SOME e =>e
            | _ => (case check(v2, ub2, v1, lb1, ub1)
            of SOME e => e
            | _ => scalarExpand (params, body, index, args)
            (* end case *))
            (* end case *)
            end
            |  _ => scalarExpand (params, body, index, args)
            (* end case *))



    (*inner product*)
    fun isInner exp = (case exp
        of E.Sum([(0,_,_)], sumexp) => (case sumexp
            of E.Opn(E.Prod, [E.Tensor(id1, [E.V 0]), E.Tensor(id2, [E.V 0])])
                => concat["T" ^ i2s id1, "•","T" ^ i2s id2]
            | _ => raise Fail "missing"
            (* end case*))
        | _ => raise Fail "missing"
    (* end case*))

    (* addition: indices must be in order*)
    fun foundAdd (e) = (case e
        of E.Const r => i2s r
        | E.ConstR r => Rational.toString r
        | E.Tensor(id, []) => "T" ^ i2s id
        | E.Tensor(id, [E.V 0]) => "T" ^ i2s id
    (* end case*))



    (*found product*)
    fun foundProd es = (case es
        (*scalar * scalar*)
        of [E.Tensor(id1,[]),E.Tensor(id2,[])]
            => concat["T" ^ i2s id1, "*","T" ^ i2s id2]
        (*mudulate*)
        |  [E.Tensor(id1, alpha1),E.Tensor(id2,alpha2)]
            => if(inOrder(alpha1,0) andalso inOrder(alpha2,0))
                then concat["modulate(T" ^ i2s id1, ",","T" ^ i2s id2,")"]
                else (raise Fail ("missing"))
    (* end case*))

    fun expToString (e,alpha) = (case e
           of E.Const r => i2s r
            | E.ConstR r => Rational.toString r
            | E.Tensor(id, []) => "T" ^ i2s id
            | E.Tensor(id, [E.V 0]) => "T" ^ i2s id
            | E.Sum _ => isInner e
            | E.Opn(E.Add, el) => concat["(", String.concatWithMap "+ "  foundAdd el,")"]
            | E.Opn(E.Prod, es) => foundProd es
            (* end case *))

    fun toString (Ein.EIN{params, index, body}) = 
        expToString (body,index)

  end
