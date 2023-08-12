let partList list startElement length = (
  let rec partListInner currentList resultList elementsToAdd indicator = (
    match (currentList, elementsToAdd) with
      | (_,0) -> List.rev resultList
      | ([],_) -> List.rev resultList
      | (head::tail, _) -> (
        if indicator then partListInner tail (head::resultList) (elementsToAdd-1) indicator
        else (
          if(head=startElement) then partListInner tail (head::resultList) (elementsToAdd-1) true
          else partListInner tail resultList elementsToAdd indicator
        )
      )
  ) in
  partListInner list [] length false
);;


open Printf
let a = partList (1::2::3::4::5::6::7::8::[]) 3 4
let () = List.iter (printf "%d ") a