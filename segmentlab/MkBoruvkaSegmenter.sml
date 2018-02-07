functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq
  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight
  type edgem = vertex * vertex * weight * int

  fun findSegments (E, n) initial_credit =
     let
      fun edgemCmp (x : edgem, y : edgem) = 
        if #3 x < #3 y then GREATER else if #3 x = #3 y then EQUAL else LESS 
      val e = sort edgemCmp (mapIdx (fn (i,(u,v,w)) => (u,v,w,i)) E)
      fun removeE (cur : int seq) (E : edgem seq) =
        filter (fn (u,v,w,_) => Int.min(nth cur u,nth cur v) - w >= 0 ) E
      fun updateC (cur : int seq) (E : edgem seq) =
        let 
          val weighttb = collect Int.compare (map (fn (_,v,w,_) => (v,w)) E)
          val weightsumtb = map (fn (k,x) => (k,reduce op+ 0 x)) weighttb
          val curtb= collect Int.compare (map (fn (u,v,_,_) => (v,nth cur u)) E)
          val curmintb = map (fn (k,x) => (k,Int.min(nth cur k,reduce Int.min (nth x 0) x)) ) curtb
        in inject (map2 (fn ((k,c),(_,w)) =>(k,c-w)) curmintb weightsumtb) cur end
      fun joinner (E : edgem seq, n : int, i : int, sd : Rand.rand) =
        let
          (*val PRINT = print ("Length E = " ^ Int.toString(length E) ^ "\n")*)
          val mapv = map (fn (u,v,w,l) => (u,(u,v,w,l))) E
          val t = tabulate (fn i => (~1,~1,~1,~1)) n
          val minE = filter (fn (_,_,_,x) => not (x = ~1)) (inject mapv t)
          val flipr = Rand.flip sd n 
          val p = filter (fn (u,v,_,_) => nth flipr u = 0 andalso nth flipr v = 1) minE
        in p end
      fun MST' (E : edgem seq, T : int seq, i : int, cur : int seq, p : int seq, sd : Rand.rand) =
        if length E = 0 then p
        else
          let
            val pt = joinner (E,n,i,sd)
            val cur' = updateC cur pt
            val pp' = inject (map (fn (u,v,_,_) => (u,v)) pt) p
            val p' = map (fn x => nth pp' x) pp'
            val T' = append (T,map (fn (_,_,_,l) => l) pt)
            val E' = map (fn (u,v,w,l) => (nth p' u,nth p' v,w,l)) (filter (fn (u,v,_,_) => not (nth p' u = nth p' v) ) E)
            val E'' = removeE cur' E'
          in MST' (E'',T',i+1,cur',p',Rand.next sd) end
      in MST' (e,(empty()),0,tabulate (fn i => initial_credit) n, tabulate (fn i => i) n, Rand.fromInt(77)) end   
end