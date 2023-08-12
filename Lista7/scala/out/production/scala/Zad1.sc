def pi(accuracy:Double, currentValue:Double):Double = {
  val nextValue = currentValue * Math.pow(0.5 + 0.5*currentValue, 0.5)
  if(Math.abs((2.0/currentValue)-(2.0/nextValue)) < accuracy) then 2.0/nextValue
  else pi(accuracy, nextValue)
}

println(pi(0.01, Math.pow(0.5,0.5)))