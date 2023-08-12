type 'a bt =  Empty | Node of 'a * 'a bt * 'a bt;;

let rec mapTree tree mapper = 
  match tree with
    | Empty -> Empty
    | Node(value, left, right) -> 
      Node(mapper value, mapTree left mapper, mapTree right mapper)
    

(*Test*)
let floatListTree = Node((1.2::2.4::3.6::[]),Node((1.2::2.6::3.1::4.7::[]),Empty,Empty),Node((1.8::[]),Empty, Empty));;
let lengthTree = Node(3,Node(4,Empty,Empty),Node(1,Empty, Empty));;
let lengthMapper list = List.length list;;

if(mapTree(floatListTree)(lengthMapper) = lengthTree) then print_string("true")
else print_string("false")


let headTree = Node(1.2,Node(1.2,Empty,Empty),Node(1.8,Empty, Empty));;
let headMapper list = List.hd list;;

if(mapTree(floatListTree)(headMapper) = headTree) then print_string("\ntrue")
else print_string("\nfalse")

