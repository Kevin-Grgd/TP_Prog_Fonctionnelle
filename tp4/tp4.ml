(* NOTE DE MUSIQUE *)

(*1*)
(*sans accumulateur*)
let rec nb_notes l_note =
match l_note with
[] -> 0
|x::r -> 1+(nb_notes r);;

nb_notes [0;1;2;0;0;1;2;0;2;3;4];;

(*avec accumulateur*)
let rec nb_notes_acc l_note acc =
match l_note with
[] -> acc
|x::r -> nb_notes_acc r (acc+1)

let nb_notes l_notes = nb_notes_acc l_notes 0;; 

nb_notes [0;1;2;0;0;1;2;0;2;3;4];;


(*2*)
let rec nb_occurences note partition =
match partition with
[] -> 0
|x::r -> if x = note
then 1+(nb_occurences note r)
else nb_occurences note r;;

nb_occurences 0 [0;1;2;0;0;1;2;0;2;3;4];;


(*3*)
let rec la_plus_aigue_bis partition aigue =
match partition with
[] -> aigue
|x::r -> if x >= aigue
then la_plus_aigue_bis r x
else la_plus_aigue_bis r aigue;;

let la_plus_aigue partition =
match partition with
[] -> failwith "la partition est vide"
|x::r -> la_plus_aigue_bis partition x;;

la_plus_aigue [0;1;2;0;0;1;2;0;2;3;4;5;6];;


(*4*)
let rec indice_plus_aigue_bis partition note i =
match partition with
[] -> 0
|x::r -> if x = note
then i
else indice_plus_aigue_bis r note (i+1);;

let indice_plus_aigue partition =
indice_plus_aigue_bis partition (la_plus_aigue partition) 0;;


(*5*)
let rec tonalite_bis partition i =
match partition with
[] -> i
|x::r -> tonalite_bis r x;;

let tonalite partition = tonalite_bis partition 0;;

tonalite [0;1;2;0;0;1;2;0;2;3;4];;


(*6*)
let rec ralentir_bis p p_ralenti =
match p with
[] -> p_ralenti
|x::r -> x::(x::(ralentir_bis r p_ralenti));;

let ralentir partition = ralentir_bis partition [];;

ralentir [0;1;2;0;0;1;2;0;2;3;4];;


(*7*)
let rec accelerer_bis p p_accelere i =
match p with
[] -> p_accelere
|x::r -> if i mod 2 = 0
then (x::(accelerer_bis r p_accelere (i+1)))
else accelerer_bis r p_accelere (i+1);;

let accelerer partition = 
accelerer_bis partition [] 0;;

accelerer [0;1;2;0;0;1;2;0;2;3;4];;


(*8*)
let rec inverser_2a2 p =
match p with
[] -> []
|x::(r::y) -> r::(x::(inverser_2a2 y))
| [x] -> [x];;

inverser_2a2 [0;1;2;0;0;1;2;0;2;3;4];;


(*9*)
let rec transcrire_bis p p_transcrit=
match p with
[] -> p_transcrit
|x::r -> if x = 0
then 5::(transcrire_bis r p_transcrit)
else if x = 1
then 6::(transcrire_bis r p_transcrit)
else (x-2)::(transcrire_bis r p_transcrit);;

let transcrire partition =
transcrire_bis partition [];;

transcrire [0;1;2;0;0;1;2;0;2;3;4];;


(*10*)
let rec gamme_bis p i =
match p with
[] -> if i > 6
then true 
else false
|x::r -> if i > 6
then gamme_bis r i
else if x=i
then gamme_bis r (i+1)
else gamme_bis r i;;

let gamme partition = gamme_bis partition 0;;

gamme [0;1;2;0;0;1;2;0;2;3;4;5;6];;


(*11*)
let rec signer_bis s partition i n =
if i = n
then partition
else if s.[i] > 'A' && s.[i] < 'Z'
then (int_of_char (s.[i])-(97-32))mod 7 ::(signer_bis s partition (i+1) n)
else (int_of_char (s.[i])-(int_of_char 'a'))mod 7 ::(signer_bis s partition (i+1) n);;

let signer s =
signer_bis s [] 0 (String.length s);;

signer "Josephine";;

(*int_of_char 'a';;
int_of_char 'a'- int_of_char 'A';;*)
