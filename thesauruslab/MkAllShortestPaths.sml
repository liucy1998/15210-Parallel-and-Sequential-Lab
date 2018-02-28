functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = (vertex seq) table
  (* { v -> <outneighbors of v> } *)
  type asp = ((vertex seq) * int) table
  (* { v -> (parent seq, distance) } *)

  fun ext x = case x of (SOME a) => a | NONE => empty()
  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph = Table.collect E
  (* Task 2.2 *)
  fun numEdges (G : graph) : int = reduce op+ 0 (map (fn x => length x) (range G)) 
  fun numVertices (G : graph) : int = Set.size (Set.union (domain G, Set.fromSeq (flatten (range G))))
  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq = ext (find G v)
  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let 
      fun bfs G masp nv dis = 
        if size nv = 0 then masp
        else
          let
            val nasp = Table.merge (fn (x,y) => x) (masp, nv)
            val subg = extract (G,(domain nv)) 
            val t = mapk (fn (x,y) => Seq.map (fn a => (a,x)) y) subg
            val nnv = Table.map (fn x => (x,dis+1)) (erase (Table.collect(flatten (range t)),domain nasp))
          in
            bfs G nasp nnv (dis+1)
          end
    in 
      bfs G (Table.empty()) ($(v,((empty()),0))) 0
    end
  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    case (Table.find A v) of NONE => (empty())
    | SOME st =>
      let
        fun retrace path dis = 
          if dis = 0 then path 
          else
            let
              fun prolong s = 
                let
                  val p = hd s
                  val parents = #1 (getOpt(Table.find A p,(empty(),1)))
                in
                  tabulate (fn i => (nth parents i)::s) (length parents)
                end
            in
              retrace (flatten (map prolong path) ) (dis-1)
            end
      in
        Seq.map (fn x => Seq.fromList x) (retrace (singleton([v])) (#2 st))
      end
end
