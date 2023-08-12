def minAndMax(list:List[List[Int]]): List[(Int, Int)] =
  def minAndMaxInner(list:List[Int], currentPair:(Int, Int)): (Int, Int) =
    (list, currentPair) match
      case (Nil,_) => currentPair
      case (head::tail, (max,min)) =>
        if head>max then minAndMaxInner(tail, (head,min))
        else if head<min then minAndMaxInner(tail, (max, head))
        else minAndMaxInner(tail, (max, min))

  list match
    case Nil => Nil
    case head::tail =>
      minAndMaxInner(head.tail, (head.head, head.head))  ::  minAndMax(tail)



println(minAndMax(List(List(1,2,3), List(3,6,9,99), List(0, 0, 3,77))))