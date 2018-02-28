
functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  exception NYI

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real
  type 'a table = 'a Table.table


  (* Define this type yourself *)
  type graph = (vertex * weight) Seq.seq table
  
  fun makeGraph (E : edge Seq.seq) : graph =  
    Table.collect (Seq.map (fn (x : edge) => (#1 x,(#2 x, #3 x))) E)
  (*A Simple modification of the Dijkstra algorithm*)
  fun findPath h G (S, T) = 
    let 
      val ts = Seq.map (fn x => (h x,(x,0.0))) (Set.toSeq S)
      val QQ = PQ.fromList (Seq.toList ts)
      fun nb G v = 
        case Table.find G v of 
          NONE => Seq.empty()
        | SOME x => x
      fun dijkstra (X,Q) = 
        case PQ.deleteMin Q of
          (NONE,_) => X 
        | (SOME (hd,(v,d)),Q') =>
          case Table.find X v of SOME _ => dijkstra (X,Q') | NONE =>
            let
              val X' = Table.insert (fn (a,b) => a) (v,d) X
              fun relax (x,(u,w)) = PQ.insert ((h u)+d+w,(u,d+w)) x 
              val Q'' = Seq.iter relax Q' (nb G v)
            in dijkstra (X',Q'') end
      val tbs = Table.toSeq (Table.extract (dijkstra (Table.empty(),QQ),T))
    in
      if Seq.length tbs = 0 then NONE
      else SOME (Seq.reduce (fn ((a,b),(c,d)) => if b < d then (a,b) else (c,d)) (Seq.nth tbs 0) tbs)
    end
end