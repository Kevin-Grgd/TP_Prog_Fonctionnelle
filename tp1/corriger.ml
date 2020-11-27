(* Géométrie *)

let la_fonction_de_mathilde z =
let a = 3.14 in
let b = 2.*.a in
b*.z;;

let la_fonction_de_dimitri x =
let p = 4.*.(atan 1.) in
let rayon = x *. x in
let w = p *. rayon in w;;

(* La suite *)

let sourire bb = 
let x = 2 in
string_of_int (x * bb) ^ "!" ^ string_of_int (x);;

let musique star = 
  let y = false and z = true in
  let bonne_musique = y && z in bonne_musique;;
