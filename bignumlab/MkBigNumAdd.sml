functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++
  datatype carry = GEN | PROP | STOP

  fun x ++ y = 
    let
      fun inv a = if a = ONE then ZERO else ONE
      fun mark (ONE, ONE) = GEN 
         |mark (ZERO, ZERO) = STOP
         |mark (_, _) = PROP
      fun align (GEN, PROP) = GEN 
         |align (STOP, PROP) = STOP
         |align (_, a) = a 
      fun add1 (ZERO, ZERO) = ZERO
         |add1 (ONE, ZERO) = ONE
         |add1 (ZERO, ONE) = ONE
         |add1 (ONE, ONE) = ZERO
      fun init a b =  
        let 
          val la = length a 
          val lb = length b 
          fun getg a b = if length a > length b then a else b
          fun addz a n = tabulate (fn i => if i < length a then nth a i else ZERO) (n+1) (* Add extra ZERO. *)
        in
          if la = lb then (append (a,singleton ZERO),append (b,singleton ZERO) )
          else if la < lb then (addz a lb, append (b,singleton ZERO))
          else (append (a,singleton ZERO), addz b la)
        end 
      val (xx,yy) = init x y
      val s = map add1 (zip xx yy)
      val cs = scani align STOP (map mark (zip xx yy) ) (* using STOP to delete PROP in the front. *)
      fun fack s (i,a) =
        if i = 0 then a
        else if nth s (i-1) = STOP andalso nth s i = GEN then a 
        else if nth s (i-1) = STOP andalso nth s i = STOP then a
        else inv a
      val t = mapIdx (fack cs) s 
    in
      if nth t ((length t)-1) = ONE then t
      else take (t,((length t)-1))  
    end    
  val add = op++
end
