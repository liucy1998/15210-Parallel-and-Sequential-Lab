functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  type 'a stseq = 'a STSeq.stseq
  open Seq

  exception NYI

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = (int * ((vertex * (vertex seq)) seq))

  fun makeGraph (E : edge seq) : ugraph = 
    if length E = 0 then (0,empty())
    else let 
      fun addRev (x : vertex * vertex) = Seq.fromList [(#1 x,#2 x),(#2 x,#1 x)]
      val t = collect Int.compare (flatten (map addRev E))
      val numv = (#1 (nth t ((length t)-1)))+1
    in (numv,t) end
  fun findBridges (G : ugraph) : edges = 
    if (#1 G) = 0 then empty()
    else
    let
      val ts = STSeq.fromSeq (tabulate (fn i => if i = 0 then 0 else ~1) ((#1 G)+1))
      fun dfs p ((TS,B,time,time_min),v) = 
        if (STSeq.nth TS v) <> ~1 then (TS,B,time,Int.min (STSeq.nth TS v,time_min))
        else
          let 
            val TS' = STSeq.update (v,time) TS
            val nb = map (fn x => x+1) (filter (fn x => x+1 <> p) (#2(nth (#2 G) (v-1))))
            val (TS'',B',time',time_min') = iter (dfs v) (TS',B,time+1,time) nb
          in 
            if time_min' >= time andalso p<> 0 then (TS'',append(B',singleton (p-1,v-1)),time',time_min)
            else (TS'',B',time',Int.min(time_min,time_min'))
          end
    in
      #2 (dfs 0 ((ts,empty(),1,0),1))
    end
end
