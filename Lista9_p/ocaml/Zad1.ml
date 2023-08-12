type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let powerLList llist = (
  let rec powerLListInner elementsToAdd pow recentElement currentList = (
    match (elementsToAdd, currentList) with
      | (_, LNil) -> currentList
      | (0, LCons(value, tail)) -> (
        match tail() with 
          | LCons(nextValue, _) -> LCons(recentElement*pow, fun() -> powerLListInner(nextValue)(nextValue)(1)(tail()))
          | _ -> LNil
      )
      | (_, LCons(value,_)) -> 
        if(elementsToAdd = pow) then LCons(recentElement*pow, fun() -> powerLListInner(elementsToAdd-1)(pow)(recentElement)(currentList))
        else LCons(recentElement*pow, fun() -> powerLListInner(elementsToAdd-1)(pow)(recentElement*pow)(currentList))
  ) in
  match llist with 
    | LCons(value, _) ->powerLListInner value value 1 llist
    | LNil -> LNil
);;

let rec toList lazyList =
  match lazyList with 
    | LNil -> []
    | LCons(head, tail) -> head::toList(tail());;


let rec toLazyList list =
  match list with
  [] -> LNil
  | h::t -> LCons(h, fun() -> toLazyList t) ;;
    

  open Printf
  let a = toList(powerLList(toLazyList([1;2;3;4;6])))
  let () = List.iter (printf "%d ") a