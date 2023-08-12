let rec fibTail(n) = (
  let rec fibTailInner(arg1, arg2, index) = (
    if(index=n-1) then arg2
    else fibTailInner(arg2, arg1 + arg2, index+1)
  ) in
  fibTailInner(0,1,0)
);;