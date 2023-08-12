def pi(accuracy:Double, currentValue:Double):Double = {
  val nextValue = currentValue * Math.sqrt(0.5 + 0.5*currentValue)
  println(currentValue)
  if(Math.abs(currentValue-nextValue) < accuracy) then 2.0/nextValue
  else pi(accuracy, nextValue)
}

println(pi(0.00000000000000000001, Math.sqrt(0.5)))