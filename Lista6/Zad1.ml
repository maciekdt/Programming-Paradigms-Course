type person = Person of string*string;;
type sex = Man | Woman;;

let findSex person = (
  match person with
    | Person(name, _) -> (
      if((String.lowercase name).[(String.length name) - 1]='a') then Woman
      else Man
    );
);;

if(findSex(Person("ANNA", "BEM")) = Woman) then print_string("true")
else print_string("false")