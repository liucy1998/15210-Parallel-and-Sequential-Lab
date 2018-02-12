functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = unit table table

  fun makeCountTable (S : point seq) : countTable = 
    let 
      fun f (tb,(x,y)) = 
        case last tb of
          NONE => $(x,$(y,()))
        | (SOME (_,ytb)) => insert #1 (x,(insert #1 (y,()) ytb)) tb
      val t = Seq.sort (fn ((x1,_),(x2,_)) => compareKey(x1,x2)) S
    in Seq.iter f (empty()) t end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRight, yLo) : point) : int  = 
    let
      val range_r = 
        case find T xRight of 
          SOME t => size (getRange t (yLo,yHi)) 
        | NONE => case previous T xRight of NONE => 0 | SOME (_,t) => size (getRange t (yLo,yHi))
      val range_l = case previous T xLeft of NONE => 0 | SOME (_,t) => size (getRange t (yLo,yHi))
    in
      range_r - range_l
    end

  
end