functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq


  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
    if p > 1.0 orelse length hist = 0 then raise Range
    else 
      let
        val t = scani (fn (a,b) => (#1 b,#2 a + #2 b) ) (#1 (nth hist 0), 0) hist
        val sum = #2 (nth t ((length t)-1) )
        val tt = map (fn (a,b) => (a,Real.fromInt(b)/Real.fromInt(sum))) t
        fun find (a : 'a * real, b : 'a * real) = if #2 a >= p then a else b 
        (* Fact : #2 a < #2 b *)
      in
        #1 (reduce find (#1 (nth hist 0),~0.1) tt)
      end
end
