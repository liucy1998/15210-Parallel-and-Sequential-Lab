functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y = 
    case (showt x, showt y) of
       (EMPTY,EMPTY) => singleton ZERO
      |(_,EMPTY) => singleton ZERO
      |(EMPTY, _) => singleton ZERO
      |(ELT _, _) => if nth x 0 = ONE then y else singleton ZERO
      |(_, ELT _) => if nth y 0 = ONE then x else singleton ZERO
      |(_,_) => 
        let
          fun delz a =
            let 
              val t = rev (map (fn ONE => 1 | ZERO => 0) a)
              val tt = scani (fn (0,1)=>1|(1,_)=>1|(0,0)=>0) 0 t
              val len = reduce op+ 0 tt
            in
              take (a, len)
            end
          fun init a b = 
            let
              val lx = length x
              val ly = length y
              fun addz a n = tabulate (fn i => if i < length a then nth a i else ZERO) n (* Add extra ZERO. *)

            in
              if lx > ly then (x, addz y lx) else (addz x ly, y)
            end
          fun mul2 a n = append(tabulate (fn _ => ZERO) n, a)
          val (xx,yy) = init x y
          val (NODE(q,p),NODE(s,r)) = (showt xx,showt yy)
          val pplusq = p++q 
          val rpluss = r++s
          val (pr, pqrs', qs) = par3 (fn _ => p**r, fn _ => pplusq**rpluss, fn _ => q**s) 
          val pqrs = (pqrs'--pr)--qs
        in
          delz ((mul2 pr (length q *2)) ++ (mul2 pqrs (length q)) ++ qs )
        end
  val mul = op**
end
