functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight
  type edgem = vertex * vertex * weight * int

  (* Remove this exception when you're done! *)
  exception NotYetImplemented

  fun MST (E : edge seq, n : int) : edge seq =
    let
      fun edgemCmp (x : edgem, y : edgem) = 
        if #3 x < #3 y then GREATER else if #3 x = #3 y then EQUAL else LESS 
      fun joinner (E : edgem seq, n : int, i : int) =
        let
          val PRINT = print ("Length E = " ^ Int.toString(length E) ^ "\n")
          val E' = sort edgemCmp E
          val mapv = map (fn (u,v,w,l) => (u,(u,v,w,l))) E'
          val t = tabulate (fn i => (~1,~1,~1,~1)) n 
          val minE = filter (fn (_,_,_,x) => not (x = ~1)) (inject mapv t)
          val flipr = Rand.flip (Rand.fromInt (12232*i)) n 
          val p = filter (fn (u,v,_,_) => nth flipr u = 0 andalso nth flipr v = 1) minE
        in p end
      fun MST' (E : edgem seq, T : int seq, i : int ) =
        if length E = 0 then T
        else
          let
            val pt = joinner (E,n,i)
            val t = map (fn (u,v,_,_) => (u,v)) pt 
            val p = inject t (tabulate (fn i => i) n)
            val T' = append (T,map (fn (_,_,_,l) => l) pt)
            val E' = map (fn (u,v,w,l) => (nth p u,nth p v,w,l)) (filter (fn (u,v,_,_) => not (nth p u = nth p v) ) E)
          in MST' (E',T',i+1) end
      val idxs = MST' (mapIdx (fn (i,(u,v,w)) => (u,v,w,i)) E,(empty()),0)
    in map (fn x => nth E x) idxs end
end
