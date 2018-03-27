

structure ReadTy =
  struct



    datatype readein = T of {
        op_otimes : string,
        op_inner : string,
        op_cross  : string,
        op_deriv : string,
        op_conv : string,
        op_comp : string,
        op_divide : string,
        op_sqrt : string,
        op_scale:string,
        op_double:string,
        op_subt:string,
        op_transpose:string,
        op_pown:string
    }

    fun get_op_otimes(T{op_otimes,...})=op_otimes
    fun get_op_inner(T{op_inner,...})=op_inner
    fun get_op_cross(T{op_cross,...})=op_cross
    fun get_op_deriv(T{op_deriv,...})=op_deriv
    fun get_op_conv(T{op_conv,...})=op_conv
    fun get_op_comp(T{op_comp,...})= op_comp
    fun get_op_divide(T{op_divide,...})= op_divide
    fun get_op_sqrt(T{op_sqrt,...})=op_sqrt
    fun get_op_scale(T{op_scale,...})=op_scale
    fun get_op_double(T{op_double,...})=op_double
    fun get_op_subt(T{op_subt,...})=op_subt
    fun get_op_transpose(T{op_transpose,...})=op_transpose
    fun get_op_pown(T{op_pown,...})=op_pown


    val op_l = (*use latex*)
        T{
            op_otimes=" \\otimes ",
            op_inner=" \\bullet ",
            op_cross= " \\times ",
            op_deriv =" \\nabla ",
            op_conv=" \\circledast " ,
            op_divide="\\frac",
            op_comp=" \\circ ",
            op_sqrt="\\sqrt",
            op_scale= "*",op_double=":",
            op_subt="-",
            op_transpose="^T",
            op_pown="^"
        }
    val op_u =(*use unicode*)
        T{
            op_otimes= "⊗",
            op_inner= "•",
            op_cross ="×",
            op_deriv ="∇",
            op_conv="⊛",
            op_divide="divide",
            op_comp="∘",
            op_sqrt="√",
            op_scale= "*",op_double=":",op_subt="-",
            op_transpose="^T",
            op_pown="^"
        }

    val op_w =(*use words*)
        T{
            op_otimes= " outer-product ",
            op_inner= " inner-product ",
            op_cross =" cross-product ",
            op_deriv =" differentiate ",
            op_conv=" convolution ",
            op_divide=" divide ",
            op_comp=" composition ",
            op_sqrt=" squareroot",
            op_scale= "scale",
            op_double="doubledot-product",
            op_subt="subtract",
            op_transpose=" tranpose",
            op_pown="power"
        }




  end
