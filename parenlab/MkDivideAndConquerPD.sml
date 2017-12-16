functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  (*
    max_length: 最远匹配距离
    l_num: 左括号剩余
    r_num: 右括号剩余
    max_l_to_right: 最左的未匹配的左括号到右边的长度
    max_r_to_left: 最右的未匹配的右括号到左边的长度
    len: 串长度
  *)
  fun parenDist (parens : paren seq) : int option =
    let
      fun fuck t=
        case showt t of
            EMPTY => (0,0,0,0,0,0)
          | ELT OPAREN => (0,1,0,1,0,1)
          | ELT CPAREN => (0,0,1,0,1,1)
          | NODE (l,r) =>
          let
            val ((max_length1,l_num1,r_num1,max_l_to_right1,max_r_to_left1,len1),
                (max_length2,l_num2,r_num2,max_l_to_right2,max_r_to_left2,len2))
              = par(fn _ => fuck l, fn _ => fuck r)
          in
            if l_num1 = r_num2 then
              let 
                val nmax_length = Int.max(max_l_to_right1+max_r_to_left2,
                                  Int.max(max_length1, max_length2))
              in 
                (nmax_length,l_num2,r_num1,max_l_to_right2,max_r_to_left1,len1+len2)
              end
            else if l_num1 < r_num2 then  
              (0,l_num2,r_num2+r_num1-l_num1,max_l_to_right2,max_r_to_left2+len1,len1+len2)
            else
              (0,l_num1+l_num2-r_num2,r_num1,max_l_to_right1+len2,max_r_to_left1,len1+len2)
          end
      val (max_length,l_num,r_num,max_l_to_right,max_r_to_left,len) = fuck parens
    in
      if len>0 andalso l_num=0 andalso r_num=0 then SOME (max_length-2)
      else NONE
    end
end
