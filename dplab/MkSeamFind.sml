functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq (* ... *)

  exception NotYetImplemented
 
  type 'a seq = 'a Seq.seq 

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun generateGradients {width, height, data} = 
    let
      fun genGrad ((row,col),(x : pixel)) = 
        if row = height-1 then 0.0
        else if col = width-1 then Real.posInf 
        else let 
          fun nthxy (s : pixel seq seq) (r,c) = nth (nth s r) c
          fun calcDiff ((x : pixel),(y : pixel)) = 
            Math.pow((#r x)-(#r y),2.0) + Math.pow((#g x)-(#g y),2.0) + Math.pow((#b x)-(#b y),2.0)
          in Math.sqrt (calcDiff(nthxy data (row,col), nthxy data (row,col+1))
                        + calcDiff(nthxy data (row,col), nthxy data (row+1,col))) end
    in mapIdx (fn (r,x) => mapIdx (fn (c,xx) => genGrad ((r,c),xx)) x) data end
  
  fun findSeam G = 
    let
      val width = length (nth G 0)
      val height = length G
      fun nthxy s (r,c) = nth (nth s r) c
      fun calc ((pre : (real * (int list)) seq),r) c = 
        let 
          val (nv,lc) = 
            if r = 0 then (nthxy G (0,c),c)
            else if c = 0 then 
              if #1 (nth pre 0) <= #1 (nth pre 1) then (#1 (nth pre 0)+(nthxy G (r,c)),0)
              else (#1 (nth pre 1)+(nthxy G (r,c)),1)
            else if c = width - 1 then
              if #1 (nth pre c) <= #1 (nth pre (c-1)) then (#1 (nth pre c)+(nthxy G (r,c)),c)
              else (#1 (nth pre (c-1))+(nthxy G (r,c)),c-1)
            else 
              if #1 (nth pre (c-1)) <= #1 (nth pre c) andalso #1 (nth pre (c-1)) <= #1 (nth pre (c+1)) 
                then (#1 (nth pre (c-1))+(nthxy G (r,c)),c-1)
              else if #1 (nth pre c) <= #1 (nth pre (c-1)) andalso #1 (nth pre c) <= #1 (nth pre (c+1)) 
                then (#1 (nth pre c)+(nthxy G (r,c)),c)
              else (#1 (nth pre (c+1))+(nthxy G (r,c)),c+1)
          in if r = 0 then (nv,[c]) else (nv,c::(#2(nth pre lc))) end
      fun genMTable (i,pre) =
        if i = height then pre
        else genMTable (i+1,tabulate (calc (pre,i)) width)
    in
       rev (fromList (#2 (reduce (fn (a,b) => if (#1 a) <= (#1 b) then a else b) 
          (Real.posInf,([])) (genMTable (0,(empty())))) ))
    end

end

