type 'a bt =  Empty | Node of 'a * 'a bt * 'a bt;;

let rec numberOfTreesInTree mainTree elementTree = (
  let rec isBeginOfTree tree1 tree2 = (
    match (tree1, tree2) with
      | (Node(value1, left1, right1), Node(value2, left2, right2)) -> (
        if(value1 != value2) then false
        else isBeginOfTree(left1)(left2) && isBeginOfTree(right1)(right2)
      )
      | (_, Empty) -> true
      | (_,_) -> false
  ) in

  match (mainTree, elementTree) with
    | (Empty, Empty) -> 1
    | (_, Empty) -> 0
    | (Node(_, left, right), _) -> (
      if(isBeginOfTree mainTree elementTree) then 1 + numberOfTreesInTree(left)(elementTree) + numberOfTreesInTree(right)(elementTree)
      else numberOfTreesInTree(left)(elementTree) + numberOfTreesInTree(right)(elementTree)
    )
    | _ -> 0
    
)

let mainTree = Node(2,
                Node(2,Node(2,Empty,Empty),Node(2,Empty,Empty)),
                Node(2,Node(2,Empty,Node(2,Empty,Empty)),Node(2,Empty,Empty)));;
let elementTree1 = Node(2, Empty, Empty);;
let elementTree2 = Node(2, Node(2,Empty,Empty), Node(2,Empty,Empty));;
let elementTree3 = Empty;;

print_int(numberOfTreesInTree mainTree mainTree);;