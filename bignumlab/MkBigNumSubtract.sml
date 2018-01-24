functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
    let
      fun init a b = 
        let
          val lx = length x
          val ly = length y
          fun addz a n = tabulate (fn i => if i < length a then nth a i else ZERO) n (* Add extra ZERO. *)
        in
          if lx > ly then (x, addz y lx) else (addz x ly, y)
        end
      val (xx,yy) = init x y
      val res = xx ++ ( (map (fn ONE => ZERO | ZERO => ONE) (append (yy,singleton ONE))) ++ singleton ONE)
    in
      take (res, length res -1)
    end
  val sub = op--
end
