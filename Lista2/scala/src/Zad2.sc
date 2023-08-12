def createIndexList[T](element:T, list:List[T]): List[Int] = {
  def createIndexListInner(elementsList:List[T], indexList:List[Int], index:Int): List[Int] = {
    elementsList match {
      case Nil => indexList.reverse
      case head::tail => {
        if(head==element) then createIndexListInner(tail, index::indexList, index+1)
        else createIndexListInner(tail, indexList, index+1)
      }
    }
  }
  createIndexListInner(list, List(), 0)
}

println(createIndexList(2, List(2,3,4,2,5,2)))