let rec create_index_list(element, list) = (
  let rec create_index_list_inner(elements_list, index_list, index) = (
    match elements_list with 
      | [] -> List.rev index_list
      | head::tail -> (
        if(head==element) then create_index_list_inner(List.tl elements_list, index::index_list, index+1)
        else create_index_list_inner(tail, index_list, index+1)
      )
  ) in
  create_index_list_inner(list, [], 0) 
);;

let list = create_index_list(2, (2::4::3::2::[]));;
print_int(List.hd list);;
print_int(List.nth list 1);;
