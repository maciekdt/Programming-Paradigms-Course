let sumRec matrix = (

  let rec sumArray_inner sum index array = 
    if(Array.length array = index+1) then sum
    else sumArray_inner (sum + Array.get array index)(index+1)(array) in

  let sumArray array = sumArray_inner 0 0 array in

  Array.map sumArray matrix
);;


(*let sumIter matrix = (
  let rec sumArray_inner array = 
    let index = ref 0 in
    let sum = ref 0 in
    while !index < Array.length array do
      sum := !sum + Array.get(array)(!index) in
      index := !index + 1 in
      ()
    done 
    index

  let sumArray array = sumArray_inner 0 0 array in

  Array.map sumArray matrix
)*)




let matrix = Array.make_matrix 3 3 0;;
Array.set(Array.get matrix 0)(0)(3);;
Array.set(Array.get matrix 0)(1)(2);;

Array.set(Array.get matrix 1)(1)(3);;

Array.set(Array.get matrix 2)(1)(7);;

let array = sumRec matrix;;
print_int(Array.get array 0);;
print_int(Array.get array 1);;
print_int(Array.get array 2);;

