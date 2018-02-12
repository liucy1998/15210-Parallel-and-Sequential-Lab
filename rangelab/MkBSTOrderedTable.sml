functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)


  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T of
      NONE => NONE
    | SOME {left = l,key = k,value = v,...} => 
      case first l of NONE => SOME (k,v) | x => x

  fun last (T : 'a table) : (key * 'a) option =
    case Tree.expose T of
      NONE => NONE
    | SOME {left = l,key = k,value = v,right = r} => 
      case last r of NONE => SOME (k,v) | x => x
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    last(#1(Tree.splitAt (T,k)))
  fun next (T : 'a table) (k : key) : (key * 'a) option =
    first(#3(Tree.splitAt (T,k)))
  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join(L,R)
  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt(T,k)
  fun getk t = case t of SOME (k,v) => k
  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let val pl = previous T low val nh = next T high in
    case (pl,nh) of
      (NONE,NONE) => T
    | (_,NONE) => #3(split (T,getk pl))
    | (NONE,_) => #1(split (T,getk nh))
    | (_,_) => #1(split (#3(split(T,getk pl)),getk nh)) end
end
