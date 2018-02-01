functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq
  type token = string
  type kgram = token seq
			
  (* You must define the abstract kgramstats type *)
  type kgramstats = int * (string hist T.table)
  
  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let
      fun pack s len = tabulate (fn i => (subseq s (i,len+1),nth s (i+len+1))) ((length s)-len-1)
      (* Generate k-grams sequence. *)
      fun strscmp (a,b) = collate String.compare(a,b)
      val tks = tokens (not o Char.isAlphaNum) corpus
      val t = flatten (tabulate (pack tks) maxK)
      val tt = append(singleton(empty(),tks), collect strscmp t)
      (* tt : <(k-grams, <tokens appeared immediately after k-grams>)> *)
    in
      (maxK,Table.fromSeq (map (fn (x,y) => (x,histogram String.compare y)) tt))
    end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq = 
    case Table.find (#2 stats) kgram of SOME x => x | NONE => empty()
  fun maxK (stats : kgramstats) : int =
    #1 stats
end
