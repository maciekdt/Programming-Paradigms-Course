module type OBSLUGA_KOLEJKI =
sig
  type 'a tk
  exception Pusta of string
  val tworz_pusta: unit -> 'a tk
  val do_kolejki: 'a * 'a tk -> 'a tk
  val z_kolejki: 'a tk -> 'a tk
  val pierwszy_element: 'a tk -> 'a
  val kolejka_pusta: 'a tk -> bool
end;;



module Kolejka:OBSLUGA_KOLEJKI =
struct
  type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
  exception Pusta of string

  let tworz_pusta() = KolejkaPusta

  let rec do_kolejki(element, kolejka) = 
    match kolejka with
      | Skladowa(glowa, ogon) -> Skladowa(glowa, do_kolejki(element, ogon))
      | KolejkaPusta -> Skladowa(element, KolejkaPusta)

  let z_kolejki kolejka = 
    match kolejka with 
      | KolejkaPusta -> KolejkaPusta
      | Skladowa(_, ogon) -> ogon

  let pierwszy_element kolejka = 
    match kolejka with 
      | KolejkaPusta -> raise(Pusta"Blad - Kolejka pusta")
      | Skladowa(glowa, _) -> glowa

  let kolejka_pusta kolejka = kolejka = KolejkaPusta

end;;



let kolejka = Kolejka.tworz_pusta();;
let kolejka = Kolejka.do_kolejki(1, kolejka);;
let kolejka = Kolejka.do_kolejki(2, kolejka);;

print_int(Kolejka.pierwszy_element kolejka);;
let kolejka = Kolejka.z_kolejki kolejka;;

let kolejka = Kolejka.do_kolejki(3, kolejka);;
let kolejka = Kolejka.do_kolejki(4, kolejka);;
let kolejka = Kolejka.do_kolejki(5, kolejka);;


print_int(Kolejka.pierwszy_element kolejka);;
let kolejka = Kolejka.z_kolejki kolejka;;

print_int(Kolejka.pierwszy_element kolejka);;
let kolejka = Kolejka.z_kolejki kolejka;;

print_int(Kolejka.pierwszy_element kolejka);;
let kolejka = Kolejka.z_kolejki kolejka;;

print_int(Kolejka.pierwszy_element kolejka);;
let kolejka = Kolejka.z_kolejki kolejka;;

print_int(Kolejka.pierwszy_element kolejka);;




