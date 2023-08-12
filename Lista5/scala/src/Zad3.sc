def checkList(list:List[Int], predicate1:Int=>Boolean, predicate2:Int=>Boolean): Boolean = {
  def checkListInner(currentList:List[Int], currentMin:Int, indicator:Boolean): Boolean = {
    currentList match {
      case Nil => indicator && predicate2(currentMin)
      case head::tail => {
        if(head>currentMin) then checkListInner(tail, currentMin, (indicator && predicate1(head)))
        else checkListInner(tail, head, (indicator && predicate1(head)))
      }
    }
  }
  checkListInner(list, list.head, true)
}


checkList(List(8,4,16,24), (a:Int)=>a%2==0, (a:Int)=>a<6)