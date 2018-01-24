functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
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
      val (xx,yy) = init x y
      val res = xx ++ ( (map (fn ONE => ZERO | ZERO => ONE) (append (yy,singleton ONE))) ++ singleton ONE)
    in
      delz (take (res, length res -1))
    end
  val sub = op--
end
