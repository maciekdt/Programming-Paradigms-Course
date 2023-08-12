let rec powerList(element, number) = (
  let rec powerListInner(list, index, value) = (
    if(index=number-1) then list @ (value*element :: [])
    else powerListInner(list @ (value*element :: []), index+1, value*element)
  ) in
  powerListInner([], 0, 1)
);;

if(powerList(2, 4)=[2;4;8;16]) then print_string("true")
else print_string("false");

