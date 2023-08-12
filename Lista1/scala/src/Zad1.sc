//let fun_a (a, int_number, float_number) = (
//  if(float_number=2.0) then (a::[]), (2+int_number::[])
//  else (a::[]), (3+int_number::[])
//  );;
//let fun_b (list1, list2) = List.hd list1 +. List.hd(List.hd(list2)) = 3.3
//let fun_c (list1, list2) = List.length (list1@list2)

def fun_a[T](a:T, int_number:Int, float_number:Float): (List[T], List[Int]) = {
  if(float_number==2.0) then (List(a), List(2+int_number))
  else (List(a), List(3+int_number))
}

def fun_b(list1:List[Float], list2:List[List[Float]]): Boolean = {
  list1.head + list2.head.head == 3.3
}

def fun_c[T](list1:List[T], list2:List[T]): Int = {
  (list1:::list2).length
}