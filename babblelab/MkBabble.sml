functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq
	   
  exception NotYetImplemented

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      fun pickRandomWord stats n seed=
        let 
          val tks = KS.lookupExts stats (empty())
          val num = length tks
          val rds = R.randomIntSeq seed (SOME (0,num)) 1
        in
         #1(nth tks (nth rds 0))
        end
      fun genSentence sd rds wdl wd cur =
        if cur = n then let val P = print ((String.concatWith " " wdl) ^ ".\n") in (String.concatWith " " wdl) ^ "." end
        else 
          let
            val nwdl = wdl @ [wd]
            val hists = KS.lookupExts stats (fromList nwdl)
          in
          if (length hists) = 0 then 
           genSentence (R.next sd) rds [] (pickRandomWord stats n (R.next sd)) 1 (* Restart when failed. *)
           (* (String.concatWith " " nwdl) ^ "." *)
          else
            genSentence (R.next sd) rds nwdl (Util.choose hists (nth rds cur)) (cur+1)
        end
      val myrds = R.randomRealSeq seed NONE n
    in
      genSentence seed myrds [] (pickRandomWord stats n seed) 1
    end
  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      fun work sd rds slist s cur = 
        if cur = n then String.concatWith " " (slist @ [s])
        else
          work (Rand.next sd) rds (slist @ [s]) (randomSentence stats (nth rds cur+1) sd) (cur+1)
      val rds = R.randomIntSeq seed (SOME (5,11)) (n+1)
    in
      work (R.next seed) rds [] (randomSentence stats (nth rds 0) seed) 0
    end
end
