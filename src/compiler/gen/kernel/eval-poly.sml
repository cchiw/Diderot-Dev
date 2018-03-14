(* support for symbolic evaluation of polynomial to generate the coefficients for kernels *)

use "rational.sml";

datatype poly = P of Rational.t list

local
  fun add' ([], coeffs2) = coeffs2
    | add' (coeffs1, []) = coeffs1
    | add' (a::coeffs1, b::coeffs2) = Rational.+(a, b) :: add'(coeffs1, coeffs2)
in
fun add (P coeffs1, P coeffs2) = P(add' (coeffs1, coeffs2))

fun subtract (P coeffs1, P coeffs2) = let
      fun sub ([], coeffs2) = List.map Rational.~ coeffs2
        | sub (coeffs1, []) = coeffs1
        | sub (a::coeffs1, b::coeffs2) = Rational.-(a, b) :: sub(coeffs1, coeffs2)
      in
        P(sub (coeffs1, coeffs2))
      end

fun multiply (P coeffs1, P coeffs2) = let
      fun mulOne (coeff, poly) = List.map (fn c => Rational.*(coeff, c)) poly
      fun shift poly = Rational.zero :: poly
      fun mul ([], _, acc) = acc
        | mul (coeff::coeffs, poly, acc) =
            mul (coeffs, shift poly, add'(acc, mulOne(coeff, poly)))
      in
        P(mul (coeffs1, coeffs2, []))
      end

fun toString (P coeffs) = let
      val d = length coeffs - 1
      fun cat (x, []) = [x]
        | cat (x, l) = x :: " + " :: l
      fun toS (_, [], l) = String.concat(List.rev l)
        | toS (d, c::r, l) = if Rational.isZero c
            then toS(d+1, r, l)
            else let
              val pow = if (d = 0) then [""]
                    else if (d = 1) then ["*x"]
                    else ["*x^", Int.toString d]
              in
                toS(d+1, r, cat(concat(Rational.toString c :: pow), l))
              end
      in
        toS (0, coeffs, [])
      end
end (* local *)

local
  val op + = add
  val op - = subtract
  val op * = multiply
  fun op / (a, b) = P[Rational./(a, b)]

  val x = P[Rational.zero, Rational.fromInt 1]  (* x^1 *)
in

val t1 = (x + 1/1)
val c4hexic = [
        69/80 + x*x*(~23/16 + x*x*(19/16 + x*(~7/12 + x*(1/16)))),
        3/160 + x*(35/8 + x*(~341/32 + x*(10/1 + x*(~147/32 + x*(25/24 - x*(3/32)))))),
        1539/160 + x*(~189/8 + x*(747/32 + x*(~12/1 + x*(109/32 + x*(~61/120 + x*(1/32))))))
      ]

end

(*
#define _C4HEXIC(x) \
  (x >= 3 \
   ? 0 \
   : (x >= 2 \
      ? 1539/160 + x*(~189/8 + x*(747/32 + x*(~12 + x*(109/32 + x*(~61/120 + x/32))))) \
      : (x >= 1 \
         ? 3/160 + x*(35/8 + x*(~341/32 + x*(10 + x*(~147/32 + x*(25/24 - x*3/32))))) \
         : 69/80 + x*x*(~23/16 + x*x*(19/16 + x*(~7/12 + x/16)))  )))
*)
