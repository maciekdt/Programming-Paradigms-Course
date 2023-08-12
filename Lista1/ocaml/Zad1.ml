let fun_a (a, int_number, float_number) = (
  if(float_number=2.0) then (a::[]), (2+int_number::[])
  else (a::[]), (3+int_number::[])
  );;
let fun_b (list1, list2) = List.hd list1 +. List.hd(List.hd(list2)) = 3.3
let fun_c (list1, list2) = List.length (list1@list2)