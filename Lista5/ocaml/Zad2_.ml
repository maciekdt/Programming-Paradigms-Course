let operation pairList operator x = (
  let compose f g x = f (g x) in
  let rec submission currentList currentFun x = (
    match currentList with 
      | [] -> currentFun x
      | (a,b)::tail -> submission(tail)(compose(currentFun)(operator a b)) x
  ) in
  submission(pairList)(fun y -> y) x;
);;

let pairList = ((fun x -> x),(fun x -> 2*x))::((fun x -> 2*x), fun x -> 2*x)::[];;
let operator a b = fun x -> ((a x)+(b x));;
let f x = operation pairList operator x;;

print_int(f 1);;