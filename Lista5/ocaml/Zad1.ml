let changeList list patternList = (
  let rec replace(currentList, element, index) = 
    if(index=0) then element::(List.tl currentList)
    else (List.hd currentList) :: replace((List.tl currentList),element,index-1)
  in

  let rec createList(resultList, leftList, leftPatternList) = 
    match (leftList, leftPatternList) with
      |([],[]) -> resultList
      |(head1::tail1, head2::tail2) -> createList(replace(resultList, head1, head2), tail1, tail2)
      |(_,_) -> resultList
  in
  createList(list, list, patternList)
);;


open Printf
let a = changeList[1;2;3;4;5;6] [2;1;4;3;0;5]
let () = List.iter (printf "%d ") a