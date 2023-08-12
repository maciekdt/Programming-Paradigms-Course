def divide(list:List[Int], value:Int): (List[Int], List[Int]) = {
  def divideInit(mainList:List[Int], list1:List[Int], list2:List[Int]): (List[Int], List[Int]) = {
    if(mainList.length==0) (list1, list2)
    else if(mainList.head>value) then divideInit(mainList.tail, list1:::List(mainList.head), list2)
    else divideInit(mainList.tail, list1, list2:::List(mainList.head))
  }
  divideInit(list, List(), List())
}

//Test
divide(List(1,3,5), 6)