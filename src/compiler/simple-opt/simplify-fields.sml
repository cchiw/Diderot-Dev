(* simplify-fields.sml
 *
 * This pass restructures the program to normalize the use of field expressions so that
 * the translation to Ein (and subsequent program transformations) can proceed cleanly.
 * The transformations are:
 *
 *      * replace "inside" tests on fields with tests on images; including turning tests
 *        that derive from multiple fields (e.g., because of field-field operations)
 *        into the conjunction of multiple tests.
 *
 * TODO:
 *
 *      * optimize away border controls for probes that are dominated by inside tests
 *
 *      * support for conditional fields/kernels/images
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure SimplifyFields : sig

    val transform : Simple.program -> Simple.program

  end = struct

    structure S = Simple
    structure V = SimpleVar
    structure VMap = V.Map
    structure Ty = SimpleTypes
    structure B = BasisVars

  (* we use a map from variables to integers to track the images that a field value is
   * defined from.  The domain of the map signifies membership, while the range is the
   * maximum support used in reconstructing field values.
   *)
    type image_set = int VMap.map
    val singleton = VMap.singleton
    val union = VMap.unionWith Int.max
    val listItems = VMap.listItemsi

  (* a property that maps kernel variables to the support of the kernel *)
    local
      val {getFn : V.t -> int, setFn, ...} =
            V.newProp (fn x => raise Fail("getSupport: no value for " ^ V.uniqueNameOf x))
    in
      val setSupport = setFn
      val getSupport = getFn
    end (* local *)

  (* a property that maps image variable to their root definition *)
    local
      val {getFn : V.t -> V.t, setFn, ...} = V.newProp (fn x => x)
    in
    val wrapImage = setFn
    val resolveImage = getFn
    end (* local *)

  (* a property that maps field variables back to a set of source image variables *)
    local
      val {getFn : V.t -> image_set, setFn, ...} =
    V.newProp (fn x => raise Fail("cat images: no images for " ^ V.uniqueNameOf x))
    in
    val bindImages = setFn
    val images = getFn
    end (* local *)

    fun isField x = (case V.typeOf x
           of (*Ty.T_Field _ => true
            | *) _ => false
          (* end case *))
 
 
 fun var x =  (V.uniqueNameOf x)
 fun ppArgs  args = String.concatWith"," (List.map var args)
 fun sp() = " "
fun ppExp (e) = let
    fun pp e = (case e
        of S.E_Var x => concat["E-var:",Ty.toString(V.typeOf x), "-",var x]
        | S.E_Lit lit=>concat["E-Literal:",Literal.toString lit]
        | S.E_Kernel k=>(Kernel.name k)
        | S.E_Select(x, fld) => concat["E-Select:",var x, ".", var fld]
        | S.E_Apply(f, args) => concat["E-Apply:",SimpleFunc.uniqueNameOf f, sp(), ppArgs ( args)]
        | S.E_Prim(f, [], args, _) => concat["E-Prime:",Var.uniqueNameOf f, sp(), ppArgs ( args)]
        | S.E_Prim(f, mvs, args, _) =>
            concat["E-Prime multiple",Var.uniqueNameOf f, sp(), ppArgs ( args)]
        | S.E_Tensor(es, _) =>  concat["E-CONSTENSOR:",ppArgs ( es)]
        | S.E_Field(es, _) => concat["E-CONSField",ppArgs ( es)]
        | S.E_Seq(es, _) =>concat["E-Sequence",ppArgs ( es)]
        | S.E_Tuple es =>concat["E-Tuple",ppArgs ( es)]
        | S.E_Project(x, i) => concat["E-Project ",var x, ".",(Int.toString i)]
        | S.E_Slice(x, indices, _) =>   concat["E-Slice",var x, "."]
        | S.E_Coerce{srcTy, dstTy, x} =>
            concat["E-Coerce",var x]
        | S.E_BorderCtl(ctl, x) =>
            concat[BorderCtl.fmt V.uniqueNameOf ctl, "(", var x, ")"]
        | S.E_LoadSeq(ty, nrrd) =>
            concat["loadSeq", Ty.toString ty]
        | S.E_LoadImage(ty, nrrd, _) =>
            concat[ "loadImage", (Ty.toString ty)]
        | S.E_InsideImage(pos, img, s) => concat[ "insideImage(", var pos, ",", var img,
        ",", (Int.toString s), ")"]

        (* end case *))
    in
        pp e
    end

         
         
  (* process an assignment by analysing the rhs expression to see if it uses a field
   * (and thus images)
   *)
    fun doAssign (lhs, e) = let
(*val _ = print(String.concat["\n\t:", ppExp (e)])*)
          fun copyImg img = (wrapImage(lhs, resolveImage img); NONE)
          fun copyFld fld = (* remy (bindImages(lhs, images fld); NONE)*) NONE
          fun image () = (* remy (bindImages(lhs, singleton(lhs, 0)); NONE)*) NONE
            fun unionArgs [] = NONE
            | unionArgs (x::xs) =NONE

(* original
fun copyImg img = (wrapImage(lhs, resolveImage img); NONE)
fun copyFld fld = (bindImages(lhs, images fld); NONE)
fun image () =    (bindImages(lhs, singleton(lhs, 0)); NONE)
fun unionArgs [] = NONE
| unionArgs (x::xs) = (
bindImages (lhs, List.foldl (fn (x, s) => union(images x, s)) (images x) xs);
NONE)
*)

          fun convolve (img, kern) = (
                bindImages (lhs, singleton(resolveImage img, getSupport kern));
                NONE)

          in
            case e
             of S.E_Var x => (case V.typeOf x
                   of Ty.T_Kernel => (setSupport(lhs, getSupport x); NONE)
                    | Ty.T_Image _ => copyImg x
                    | Ty.T_Field _ => copyFld x
                    | _ => NONE
                  (* end case *))
              | S.E_Lit _ => NONE
              | S.E_Kernel h => (setSupport(lhs, Kernel.support h); NONE)
              | S.E_Select _ => NONE
              | S.E_Apply _ => NONE
              | S.E_Prim(rator, _, args, _) =>
                  if Var.same(rator, B.convolve_vk)
                    then (print" conv-vk";let val [img, kern] = args in convolve (img, kern) end)
                  else if Var.same(rator, B.convolve_kv)
                    then (print" conv-kv";let val [kern, img] = args in convolve (img, kern) end)
                 else if Var.same(rator, B.kernels_kkk)
                    then (setSupport(lhs, getSupport (List.hd(args))); NONE)
                else if Var.same(rator, B.kernels_kk)
                    then (setSupport(lhs, getSupport (List.hd(args))); NONE)


(* TODO: handle probes for conditional fields


                  else if Var.same(rator, B.op_probe)
                    then ??
*)
(*
                  else if Var.same(rator, B.fn_inside)

                    then (print" \ninside";let
                    (* we convert an inside operator to one or more inside tests on images *)
                      val [pos, fld] = args
                      in
case (print"pre list items";listItems(images fld))
                         of [] => (*raise Fail "no image for inside test"*)
                                          SOME(S.E_InsideFEM(pos, fld), [])
                          | [(img, s)] => SOME(S.E_InsideImage(pos, img, s), [])
                          | (img, s)::imgs => let
                              fun mkTest ([], exp, stms) = SOME(exp, stms)
                                | mkTest ((img, s)::imgs, exp, stms) = let
                                    val b1 = V.new("isIn", V.LocalVar, Ty.T_Bool)
                                    val b2 = V.new("isIn", V.LocalVar, Ty.T_Bool)
                                    val stms =
                                          S.S_Var(b1, SOME(S.E_InsideImage(pos, img, s))) ::
                                          S.S_Var(b2, SOME exp) :: stms
                                    val exp' = S.E_Prim(B.and_b, [], [b2, b1], Ty.T_Bool)
                                    in
                                      mkTest (imgs, exp', stms)
                                    end
                              in
                                mkTest (imgs, S.E_InsideImage(pos, img, s), [])
                              end
                        (* end case *)
                      end)
*)

(*
                    else if Var.same(rator, B.fn_convert_fvp)
then (print "\nconvert f";NONE)
                    else if Var.same(rator, B.fn_convert_fp)
                        then (print "\nconvert  rm ";NONE)
                    else if Var.same(rator, B.op_probe)
                        then (print "\nprobe";NONE)
*)
                    else (print "other";
                    unionArgs (List.filter isField args))
| S.E_Field (args,_) => (print"e-fld";unionArgs (List.filter isField args))
| S.E_Tensor _ => (print"e-tens";NONE)
              | S.E_Seq _ => NONE
(* WARNING: if tuples become a surface-language feature, then we will need to track tuples
 * that contain fields or images.
 *)
              | S.E_Tuple xs => NONE
              | S.E_Project(x, i) => NONE
              | S.E_Slice(x, _, _) => if isField x then copyFld x else NONE
              | S.E_Coerce{x, ...} => (case V.typeOf x
                   of Ty.T_Kernel => (setSupport(lhs, getSupport x); NONE)
                    | Ty.T_Field _ => copyFld x
                    | _ => NONE
                  (* end case *))
              | S.E_BorderCtl(_, x) => copyImg x
              | S.E_LoadSeq _ => NONE
              | S.E_LoadImage _ => image()
              | S.E_InsideImage _ => raise Fail "premature InsideImage"
            (* end case *)
          end

    fun doBlock (S.Block{code, props}) = let

            fun stmtToString stm = (case stm
                of S.S_Var(x, SOME e) =>
concat["S-VAR:",Ty.toString(SimpleVar.typeOf x), SimpleVar.uniqueNameOf x,":",ppExp(e)]
                | S.S_Var(x, None) =>  concat["S-VAR:",Ty.toString(SimpleVar.typeOf x),SimpleVar.uniqueNameOf x,":","none"]
                | S.S_Assign(x, e) =>  concat["S-Assign:",Ty.toString(SimpleVar.typeOf x), SimpleVar.uniqueNameOf x,":",ppExp(e)]
                | S.S_IfThenElse(x, blk1,blk2) => concat["S-IFTHEN ELSE-var:",Ty.toString(SimpleVar.typeOf x),"if\n\t then",blkToString blk1,"\n\telse",blkToString blk2]
                | S.S_Foreach _ =>concat["S-for each"]
                | S.S_New _ => concat["S- new"]
                | S.S_KillAll => "S-kill_all;"
                | S.S_StabilizeAll => "S-stabilize_all;"
                | S.S_Continue =>"S-continue;"
                | S.S_Die =>  "S-die;"
                | S.S_Stabilize => "S-stabilize"
                (* end case *))
            and  blkToString(S.Block{code, ...})  =
                String.concatWith "\n"(List.map (fn e1 => "\n"^stmtToString(e1))  code)


fun doStmt (stm, stms) = ( print("\n\t-"^stmtToString(stm));case stm
                 of S.S_Var(x, SOME e) => (case doAssign (x, e)
                       of SOME(e', stms') => S.S_Var(x, SOME e') :: stms' @ stms
                        | NONE => stm::stms
                      (* end case *))
                  | S.S_Assign(x, e) => (*stm::stms*)

                        (case doAssign (x, e)
                       of SOME(e', stms') => S.S_Assign(x, e') :: stms' @ stms
                        | NONE => stm::stms
                      (* end case *))

                | S.S_IfThenElse(x, blk1, blk2) => (print" made it simplify-fields";
                      S.S_IfThenElse(x, doBlock blk1, doBlock blk2) :: stms)
                  | S.S_Foreach(x, seq, blk) =>
                      S.S_Foreach(x, seq, doBlock blk) :: stms
                  | S.S_MapReduce mrs => let
                      fun doMR (S.MapReduce{result, reduction, mapf, args, source, domain}) = let
                            val S.Func{f, params, body} = mapf
                            val body' = doBlock body
                            in
                              S.MapReduce{
                                  result = result,
                                  reduction = reduction,
                                  mapf = S.Func{f = f, params = params, body = body'},
                                  args = args,
                                  source = source,
                                  domain = domain
                                }
                            end
                      in
                        S.S_MapReduce(List.map doMR mrs) :: stms
                      end
                  | _ => stm::stms
                (* end case *))
          val stms = List.foldl doStmt [] code
          in
            S.Block{code = List.rev stms, props = props}
          end

    fun doInput (Inputs.INP{var, ty = APITypes.ImageTy _, ...}) =
          bindImages(var, singleton(var, 0))
      | doInput _ = ()

    fun doFunc (S.Func{f, params, body}) = S.Func{f = f, params = params, body = doBlock body}

    fun transform prog = let
          val S.Program{
                  props, consts, inputs, constInit, globals, globInit, funcs, strand,
                  create, start, update
                } = prog
          val S.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          in
          (* first handle input images *)
            List.map doInput inputs;
            S.Program{
              props = props,
              consts = consts,
              inputs = inputs,
              constInit = doBlock constInit,
              globals = globals,
              globInit = doBlock globInit,
              funcs = List.map doFunc funcs,
              strand = S.Strand{
                  name = name,
                  params = params,
                  spatialDim = spatialDim,
                  state = state,
                  stateInit = doBlock stateInit,
                  startM = Option.map doBlock startM,
                  updateM = doBlock updateM,
                  stabilizeM = Option.map doBlock stabilizeM
                },
              create = Create.map doBlock create,
              start = Option.map doBlock start,
              update = Option.map doBlock update
            }
          end

  end
