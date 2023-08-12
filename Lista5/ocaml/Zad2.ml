let operation pairList operator x = (
  let compose f g x = f (g x) in
  let rec submission currentList currentFun x = (
    match currentList with 
      | [] -> currentFun x
      | (a,b)::tail -> submission(tail)(compose(currentFun)(operator a b)) x
  ) in
  submission(pairList)(fun y -> y) x;
);;

let pairList = (1,2)::(2,2)::[];;
let operator a b = fun x -> x*(a+b);;
let f x = operation pairList operator x;;

print_int(f 1);;



