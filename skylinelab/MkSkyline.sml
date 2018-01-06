functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    case showt buildings of 
      EMPTY => empty()
    | ELT (l,h,r) => fromList [(l,h),(r,0)]
    | NODE (b1,b2) =>
      let 
        fun mark (b : int) (a: (int * int))  = (#1 a, #2 a, b)
        fun inv (a : (int * int * int)) = (#1 a, #2 a)
        fun calc (p : int)((a:(int * int * int)),(b:(int * int * int))) = 
          if #3 b = p then (#1 b, #2 a, #3 a) else b
        fun cmp (x,y) = 
          let val (a,_,_) = x val (b,_,_) = y in
            if a < b then LESS 
            else if a = b then EQUAL 
            else GREATER end
        val (l',r') = par (fn _ => skyline b1, fn _ => skyline b2)
        val l = map (mark 1) l' 
        val r = map (mark ~1) r'
        val kkk = merge cmp l r
        (* The height of elements in group 1 can only be changed by those in
           group -1. Vice versa. *)
        val s = map2 (fn ((a,b,c),(_,d,_)) => (a,Int.max(b,d),c)) 
            (scani (calc 1) (0,0,1) kkk) (scani (calc ~1) (0,0,~1) kkk)
      in 
        map inv (filterIdx (fn (i,p) => if i = 0 then true else #2 (nth s (i-1)) <> #2 p) s)
      end
end
