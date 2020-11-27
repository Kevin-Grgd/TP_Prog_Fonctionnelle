(*1*)
let n = 1 + 2 + 3 + 4 + 5;;

(*2*)
let m = float_of_int(n*n*n);;

(*3*)
let n = 1 * 2 * 3 * 4 * 5;;
m;;

(*4*)
n - n;;
(* x - x;; *)
   
(*5*)
n;;
let n = 30 in (n * n * n);;
n;;

(*6*)
let x = 156 and y = 123 in
let a=1+x*y and b=x-y and c=x+2*y in b*b-4*a*c;;

(*7*)
let pi = 3.14 in
let r = 2. +. 6. *. 1.5 in
2. *. pi *. r;;

(*8*)
let q=5/2 and r=5 mod 2;;

(*9*)
let val_reel = 5. /. 2.;;

(*10*)
9;;
'9';;
"9";;
9.0;;

(*11*)
let pi_s = "-3.14";;
let pi_f = float_of_string(pi_s);;

(*12*)
(* let pi_i = int_of_string(pi_s);; *)
let pi_i = int_of_float(float_of_string(pi_s));;

(*13*)
let pi_s2 = pi_s ^ "159";;
let pi_f2 = float_of_string(pi_s2);;
let pi_i2 = int_of_float(pi_f2);;

(*14*)
let c = "La pie voleuse";;
let l = String.length(c);;

(*15*)
let debut = c.[0] and fin = c.[l-1];;


(*4 FONCTION DE COMPARAISON*)

let comparaison p1 p2 p3 p4 p5 =
if (float_of_int p1) +. p2 > 10.0 || ((String.length p5) - p4) <> 0 && p3 <= 'd'
then true
else false


(*5 OU EXLUSIF*)

let ou_exclusif p1 p2 =
if p1=true || p2=true
then true
else false


(*6 CONDITIONELLES*)

(*1*)

let majorite nom age =
if age>=16 && (nom="Bresil" || nom="Equateur" || nom="Autriche")
then "Vous pouvez voter"
else if age<16 &&  (nom="Bresil" || nom="Equateur" || nom="Autriche")
then "Vous ne pouvez pas voter"
else if age>=18 && (nom="France" || nom="Inde" || nom="Egypte")
then "Vous pouvez voter"
else if age<18 && (nom="France" || nom="Inde" || nom="Egypte")
then "Vous ne pouvez pas voter"
else if age>=20 && (nom="Cameroun" || nom="Japon")
then "Vous pouvez voter"
else if  age<20 && (nom="Cameroun" || nom="Japon")
then "Vous ne pouvez pas voter"
else failwith"On sait pas bro";;

majorite "Japon" 19;;

(*2*)

let signe_produit p1 p2 =
if (p1 < 0 && p2 > 0)  || (p1 > 0 && p2 <0)
then "Leur produit est negatif"
else "Leur produit est positif";;

signe_produit (-8) 9;;


(*7 PLUS PETIT ENTIER*)

(*1*)

let min2 a b =
if a<b
then a
else b;;

let max2 a b =
if a<b
then b
else a;;

(*2*)

let min3 a b c =
min2 (min2 a b) c;;

let max3 a b c =
max2 (max2 a b) c;;

(*3*)

let plus_petit_entier a b c =
(min3 a b c)*100+ ((a + b +c)-(min3 a b c)-(max3 a b c))*10 + (max3 a b c);;
plus_petit_entier 1 6 8;;
