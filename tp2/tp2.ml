(*1 DES NOTES*)

(*1*)

let moyenne_2_notes note1 note2 =
(note1 +. note2)/. 2.;;

moyenne_2_notes 20. 0.;;
"---------------------------------------------------------------"
(*2*)

let moyenne_3_notes note1 note2 note3 =
(note1 +. note2 +. note3) /. 3.;;

moyenne_3_notes 0. 10. 20.;;
"---------------------------------------------------------------"
(*3*)

let moyenne_informatique note1 note2 note3 =
((note1 *. 3.) +. note2 +. note3) /. 5.;; 

moyenne_informatique 12. 16. 16.;; 
"---------------------------------------------------------------"
(*4*)

let moyenne_generale c n1 n2 n3 n4 n5 n6 n7 n8 n9 n10=
if c = 'I'
then (((moyenne_3_notes n1 n2 n3)*. 5.)+.((moyenne_2_notes n4 n5)*. 3.)+.((moyenne_2_notes n6 n7)*. 3.)+.((moyenne_informatique n8 n9 n10)*. 7.))/. 18.
else if c = 'M'
then (((moyenne_3_notes n1 n2 n3)*. 7.)+.((moyenne_2_notes n4 n5)*. 3.)+.((moyenne_2_notes n6 n7)*. 3.)+.((moyenne_informatique n8 n9 n10)*. 5.))/. 18.
else failwith "erreur filière";;


(*5*)

moyenne_generale 'I' 10. 6. 9. 11. 12. 9. 12.5 8. 15. 14.;;
moyenne_generale 'M' 10. 6. 9. 11. 12. 9. 12.5 8. 15. 14.;;
"---------------------------------------------------------------"
(*6*)

let rattrapage_mathematiques n1 n2 n3 n4 n5 n6 n7 =
(180. -. (((moyenne_2_notes n1 n2)*.3.) +. ((moyenne_2_notes n3 n4)*.3.) +. ((moyenne_informatique n5 n6 n7)*.5.))) /. 7.;;

rattrapage_mathematiques 11. 12. 9. 12.5 8. 15. 14.;; 
"---------------------------------------------------------------"
(*7*)

let rattrapage_mathematiques_2 n1 n2 n3 n4 n5 n6 n7 nm =
((nm *. 18.) -. (((moyenne_2_notes n1 n2)*.3.) +. ((moyenne_2_notes n3 n4)*.3.) +. ((moyenne_informatique n5 n6 n7)*.5.))) /. 7.;;

rattrapage_mathematiques_2 11. 12. 9. 12.5 8. 15. 14. 11.;;
rattrapage_mathematiques_2 11. 12. 9. 12.5 8. 15. 14. 12.;;

"---------------------------------------------------------------"
(*2 BOÎTE DE NUIT*)

(*1*)

let prix_entree age sexe =
if age > 25
then 12.
else if age >= 18 && age <=25
then if (sexe = 'f' || sexe = 'F')
then (12. *. 0.8) /. 2.
else (12. *. 0.8)
else failwith "Vous n'etes pas majeur !";;

prix_entree 19 'f';;

"---------------------------------------------------------------"
(*2*)

let prix_entree_anniversaire age sexe =
(prix_entree age sexe) *. (1. -. (float_of_int(age) /. 100.));;

prix_entree_anniversaire 26 'F';;

"---------------------------------------------------------------"
(*3 DES SECONDES*)

(*1*)

let conversion h m s =
h*3600 + m*60 + s;;

conversion 2 30 10;;

(*2*)
let temps_ecoule h1 m1 s1 h2 m2 s2 =
(conversion h2 m2 s2) - (conversion h1 m1 s1);;

temps_ecoule 2 30 10 3 0 0;;

"---------------------------------------------------------------"
(*4 JEU DE FLECHETTES*)

let gain_flechette x y =
let x = abs_float(x) and y = abs_float(y) in
if sqrt(x*.x +. y*.y) <= 2.
then 100
else if sqrt(x*.x +. y*.y) <= 4.
then 75
else if sqrt(x*.x +. y*.y) <= 6.
then 50
else if sqrt(x*.x +. y*.y) <= 8.
then 25
else failwith "Rate !";;

gain_flechette 2. 2.;;

"---------------------------------------------------------------"
(*5 DECOMPOSITION DE JETES DE DES*)

let nb_3 nb_init =
nb_init / 3;;

nb_3 6;;

let nb_2 nb_init =
(nb_init mod 3)/2;;

nb_2 8;;

let nb_1 nb_init =
(nb_init mod 3) mod 2;;

nb_1 5;;


let decompose_des nb_init =
string_of_int(nb_3 nb_init)^" jetes valant 3, "^string_of_int(nb_2 nb_init)^" jetes valant 2, "^string_of_int(nb_1 nb_init)^" jetes valant 1.";;

decompose_des 13;;
