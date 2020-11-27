(* EXERCICE 2 : DES STRINGS AUX GRANDS ET VICE VERSA *)

(*1*)
let chiffre_of_char c =
if c = '0'
then 0
else if c = '1'
then 1
else if c = '2'
then 2
else if c = '3'
then 3
else if c = '4'
then 4
else if c = '5'
then 5
else if c = '6'
then 6
else if c = '7'
then 7
else if c = '8'
then 8
else if c = '9'
then 9
else failwith"ce n'est pas un chiffre";;

"---------------------------------------------------------------"

(*2*)
let rec grand_of_string_bis s i l =
if i = String.length s
then l
else if (s.[i] = '0') && String.length s = 1
then grand_of_string_bis s (i+1) l
else if s.[i] = '-'
then failwith "erreur chiffre_of_char"
else if s.[i] = ' '
then failwith "erreur chiffre_of_char"
else grand_of_string_bis s (i+1) ((chiffre_of_char s.[i])::l);;

let rec suppr_zero_string_bis s i n =
if i = n
then ""
else if s.[i] = '0'
then suppr_zero_string_bis s (i+1) n
else (String.sub s i (n-i));;

let suppr_zero_string s = suppr_zero_string_bis s 0 (String.length s);;

let grand_of_string s = 
let sans_zero = suppr_zero_string s in
grand_of_string_bis sans_zero 0 [];;

"---------------------------------------------------------------"

(*3*)
let string_of_chiffre i =
if i = 0
then "0"
else if i = 1
then "1"
else if i = 2
then "2"
else if i = 3
then "3"
else if i = 4
then "4"
else if i = 5
then "5"
else if i = 6
then "6"
else if i = 7
then "7"
else if i = 8
then "8"
else if i = 9
then "9"
else failwith "ce n'est pas un chiffre";;


(*4*)
let rec string_of_grand_bis l s =
match l with
[]-> s
|x::r -> string_of_grand_bis r ((string_of_chiffre x)^s);;

let string_of_grand l = string_of_grand_bis l "";;

"---------------------------------------------------------------"

(*EXERCICE 3 OPERATIONS DE COMPARAISON*)

(*1*)
let inf_strict l1 l2 =
List.rev l1 < List.rev l2;;

"---------------------------------------------------------------"

(*2*)
let inf_egal l1 l2 =
List.rev l1 <= List.rev l2;;

"---------------------------------------------------------------"

(*3*)
let sup_strict l1 l2 =
List.rev l1 > List.rev l2;;

"---------------------------------------------------------------"

(*4*)
let sup_egal l1 l2 =
List.rev l1 >= List.rev l2;;

"---------------------------------------------------------------"


(*EXERCICE 4 ARITHMETIQUE DES GRANDS*)

(*1*)

let rec successeur l =
grand_of_string (string_of_int(int_of_string(string_of_grand l) + 1));;

"---------------------------------------------------------------"

(*2*)
let additionne l1 l2 =
grand_of_string (string_of_int(int_of_string(string_of_grand l1) + int_of_string(string_of_grand l2)));;

"---------------------------------------------------------------"

(*3*)

let predecesseur l =
if l = []
then failwith "erreur predecesseur"
else grand_of_string (string_of_int(int_of_string(string_of_grand l) - 1));;

"---------------------------------------------------------------"

(*4*)
let soustrait l1 l2 =
let l1 = (int_of_string(string_of_grand l1)) and l2 = (int_of_string(string_of_grand l2)) in
if l1 < l2
then failwith "erreur soustrait"
else grand_of_string(string_of_int(l1 - l2));;

"---------------------------------------------------------------"
