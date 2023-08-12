def root(accurancy:Double, n:Double, number:Double): Double = {
  def root_inner(a:Double, b:Double, mid:Double, lastMid:Double): Double = {
    if(math.abs(mid-lastMid)<accurancy) then mid
    else if(math.pow(mid,n)>number) then root_inner(a, mid, (a+mid)/2.0, mid)
    else root_inner(mid, b, (b+mid)/2.0, mid)
  }
 if number>=1 then root_inner(-number, number, 0, number)
 else root_inner(number, 1, (number+1)/2, number)
}

println(root(0.00001, 3.0, 0.5))
