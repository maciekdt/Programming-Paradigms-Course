def operation(n:Int, x:Double, sign:Int, counter:Int, factorialValue:Int, powerValue:Double):  Double = {
  if n==counter then powerValue*sign/factorialValue
  else powerValue*sign/factorialValue + operation(n, x, sign*(-1), counter+1, factorialValue*(counter+1), powerValue*x)
}

def series(n:Int, x:Double): Double = {
  operation(n, x, -1, 1, 1, x)
}

//TEST
println(series(5, 3.0))