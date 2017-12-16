functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
    let
      fun fuck (a : int, b: int) = if a>=b then a else b
      fun check (cnt : int, cur : int) : bool =
        let 
          val len = Seq.length parens 
        in
          case cur = len of 
            true => if cnt=0 then true else false
          | false => 
            case nth parens cur of
              OPAREN => check (cnt+1, cur+1)
            | CPAREN => if cnt-1 < 0 then false else check (cnt-1,cur+1)
        end
      fun work (ans : int, cnt : int, cur : int) : int =
        if cur = Seq.length parens then ans
        else
          case nth parens cur of
            OPAREN => if ans = ~1 then work(0,1,cur+1) else work(ans+1,cnt+1,cur+1)
          | CPAREN => if ans = ~1 then 0 
                      else if cnt=1 then ans
                      else work(ans+1,cnt-1,cur+1)
      fun getans (cur : int, ans : int) : int = 
        let
          val len = Seq.length parens 
        in
          if cur < len then 
            let
              val nans = fuck(work(~1,~1,cur), ans)
            in
              getans (cur+1, nans)
            end
          else ans
        end
    val ans = getans(0,0)
    in
      if check(0, 0) then SOME ans
      else NONE
    end
end