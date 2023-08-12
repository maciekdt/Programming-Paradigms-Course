type 'a tree = Leaf of 'a | BinNode of 'a tree * 'a tree | MonoNode of 'a tree ;;

let rec checkTree tree predicate = (
  match tree with
    | Leaf(value) -> predicate value
    | MonoNode(child) -> checkTree child predicate
    | BinNode(child1, child2) -> ((checkTree child1 predicate) || (checkTree child2 predicate))
);;

let testTree = BinNode(BinNode(Leaf(2), Leaf(4)), MonoNode(Leaf(6)));;
if(checkTree testTree (fun x -> x=4)) then print_string("true")
else print_string("false")