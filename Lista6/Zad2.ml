type car = Car of string*string*int;;


let findAvarage carsList = (
  let rec sumYears currentList sum = (
    match currentList with
      | [] -> sum
      | Car(_,_,year)::tail -> sumYears(tail)(sum+year)
  ) in
  (sumYears carsList 0)/(List.length carsList)
);;

let carsList = Car("Opel", "astra", 1999) :: Car("Renault", "megane", 2004) :: Car("Toyota", "avensis", 2009) :: Car("Nissan", "micra", 2003) :: [];;

print_int(findAvarage carsList)