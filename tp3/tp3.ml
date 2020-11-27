(*1 POUR BIEN COMMENCER*)

(*1*)
let rec existe_g_d_bis c s i =
if i = (String.length s)
then false
else if c = s.[i]
then true
else existe_g_d_bis c s (i+1);;

let existe_g_d c s =
if String.length s = 0
then failwith"erreur"
else existe_g_d_bis c s 0;;

existe_g_d 'a' "Bonjoura";;

"---------------------------------------------------------------"

(*2*)
let rec existe_d_g_bis c s i =
if i < 0
then false
else if c = s.[i]
then true
else existe_d_g_bis c s (i-1);;

let existe_d_g c s =
if String.length s = 0
then failwith"erreur"
else existe_d_g_bis c s ((String.length s)-1);;

existe_d_g 'a' "aBonjour";;

"---------------------------------------------------------------"

(*3*)
let rec nb_occurences_g_d_bis c s i =
if i = String.length s
then 0
else if c = s.[i]
then 1+(nb_occurences_g_d_bis c s (i+1))
else nb_occurences_g_d_bis c s (i+1);;

let nb_occurences_g_d c s =
if String.length s = 0
then failwith"erreur"
else nb_occurences_g_d_bis c s 0;;

nb_occurences_g_d 'a' "Bajouartegdfgdkahfhaahfgkahsdkhaa";;

"---------------------------------------------------------------"

(*4*)
let rec nb_occurences_d_g_bis c s i =
if i = 0
then 0
else if c = s.[i]
then 1+(nb_occurences_d_g_bis c s (i-1))
else nb_occurences_d_g_bis c s (i-1);;

let nb_occurences_d_g c s =
if String.length s = 0
then failwith"erreur"
else nb_occurences_d_g_bis c s ((String.length s)-1);;

nb_occurences_d_g 'a' "Bajouartegdfgdkahfhaahfgkahsdkhaa";;

"---------------------------------------------------------------"

(*2 LA DISPARITION*)

(*1*)
let est_lipogramme c s =
not(existe_g_d c s);;

est_lipogramme 'a' "Bonjour";;

"---------------------------------------------------------------"

(*2*)
(* Parcours gauche à droite *)
let rec lipogramme_g_d_bis s c i s2 n =
if i = n
then s2
else if c = s.[i]
then lipogramme_g_d_bis s c (i+1) s2 n
else lipogramme_g_d_bis s c (i+1) (s2^String.make 1 s.[i]) n;;

let lipogramme s c =
if String.length s = 0
then failwith"erreur"
else lipogramme_g_d_bis s c 0 "" (String.length s);;

lipogramme "Le chat et la mouettei" 'e';;

(* Parcours de droite à gauche *)
let rec lipogramme_d_g_bis s c i s2 =
if i < 0
then s2
else if c = s.[i]
then lipogramme_d_g_bis s c (i-1) s2 
else lipogramme_d_g_bis s c (i-1) (String.make 1 s.[i]^s2) ;;

let lipogramme_d_g s c =
if String.length s = 0
then failwith"erreur"
else lipogramme_d_g_bis s c ((String.length s)-1) "";;

lipogramme_d_g "Le chat et la mouette danse le zouk, la macarena et la salsa betement" 'e';;

"---------------------------------------------------------------"

(*3 JEUX DE MOTS*)

(*1 Version Classqiue*)

(*1*)
let rec est_palindrome_bis s i u =
if u = 0
then true
else if s.[i]=s.[u]
then est_palindrome_bis s (i+1) (u-1)
else false;;

let est_palindrome s =
est_palindrome_bis (lipogramme s ' ') 0 ((String.length (lipogramme s ' '))-1);;

est_palindrome "sagas";;

"---------------------------------------------------------------"

(*2*)
let rec est_prefixe_bis s1 s2 i1 i2 =
if i1 = String.length s1
then true
else if s1.[i1] = s2.[i2]
then est_prefixe_bis s1 s2 (i1+1) (i2+1)
else false;;

let est_prefixe s1 s2 =
if (String.length s1) > (String.length s2)
then failwith"La premiere chaine de caractere est plus longue que la deuxieme."
else est_prefixe_bis (lipogramme s1 ' ') (lipogramme s2 ' ') 0 0;;

est_prefixe "Bonjour" "Hey, bonjour je s'appelle Enrico";;
est_prefixe "Hey, bonjour" "Hey, bonjour je s'appelle Enrico";;

"---------------------------------------------------------------"

(*3*)
let rec est_suffixe_bis s1 s2 i1 i2 =
if i1 < 0
then true
else if s1.[i1] = s2.[i2]
then est_suffixe_bis s1 s2 (i1-1) (i2-1)
else false;;

let est_suffixe s1 s2 =
if String.length s1 > String.length s2
then failwith"La premiere chaine de caractere est plus longue que la deuxieme."
else est_suffixe_bis (lipogramme s1 ' ') (lipogramme s2 ' ') ((String.length (lipogramme s1 ' '))-1) ((String.length (lipogramme s2 ' '))-1);;

est_suffixe "je s'appelle Enrico" "Hey, bonjour je s'appelle Enrico";;
est_suffixe "je m'appelle Enrico" "Hey, bonjour je s'appelle Enrico";;

"---------------------------------------------------------------"

(*4*)
let rec suppr_lettre_bis s1 s2 i n=
if i = n
then s2
else suppr_lettre_bis s1 (s2^(String.make 1 s1.[i+1])) (i+1) n;;

let suppr_lettre s =
suppr_lettre_bis s "" 0 ((String.length s)-1);;

suppr_lettre "Je m'appelle Kevin";;

let rec est_facteur_bis s1 s2 =
if String.length s1 > String.length s2
then false
else if est_prefixe s1 s2
then true
else est_facteur_bis s1 (suppr_lettre s2);;


let est_facteur s1 s2 =
est_facteur_bis (lipogramme s1 ' ') (lipogramme s2 ' ');;

est_facteur "ab" "acabd";;
est_facteur "ac" "agcabd";;

(*5*)
let rec est_sous_mot_bis s1 s2 i1 i2 =
if i1 = String.length s1
then true
else if i2 = String.length s2
then false
else if s1.[i1] = s2.[i2]
then est_sous_mot_bis s1 s2 (i1+1) (i2+1)
else est_sous_mot_bis s1 s2 i1 (i2+1);;

let est_sous_mot s1 s2 =
if String.length s1 > String.length s2
then false
else est_sous_mot_bis (lipogramme s1 ' ') (lipogramme s2 ' ') 0 0;;

est_sous_mot "ab" "mea tdbac";;
est_sous_mot "ab" "cboac";;

"---------------------------------------------------------------"

(*3 Mots Entremeles*)

(*1*)
let rec mots_meles_bis s1 s2 i1 i2 s3 =
if (i1 = String.length s1) && (i1=i2) 
then s3
else if i1 > i2
then mots_meles_bis s1 s2 i1 (i2+1) (s3^(String.make 1 s2.[i2])) 
else mots_meles_bis s1 s2 (i1+1) i2 (s3^(String.make 1 s1.[i1]));;

let mots_meles s1 s2 =
if String.length s1 = String.length s2
then mots_meles_bis s1 s2 0 0 ""
else failwith "Les chaines ne font pas la meme longueur";;

mots_meles "shoe" "cold";;


(*2*)
let rec mots_meles2_bis s1 s2 i1 i2 s3 =
if (i1 = String.length s1) && (i2 = String.length s2)
then s3
else if i1 = String.length s1
then mots_meles2_bis s1 s2 i1 (i2+1) (s3^String.make 1 s2.[i2])
else if i2 = String.length s2
then mots_meles2_bis s1 s2 (i1+1) i2 (s3^String.make 1 s1.[i1])
else if i1 > i2
then mots_meles2_bis s1 s2 i1 (i2+1) (s3^String.make 1 s2.[i2])
else mots_meles2_bis s1 s2 (i1+1) i2 (s3^String.make 1 s1.[i1])


let mots_meles2 s1 s2 =
if String.length s1 = String.length s2
then mots_meles s1 s2
else mots_meles2_bis s1 s2 0 0 "";;

mots_meles2 "shoeabc" "cold";;
mots_meles2 "shoe" "coldxyz";;

let test_bis s s2 =
s2^(String.sub s 12 35);;

let test s =
test_bis s "";;

test "Salut a tous, c'est David Lafarge Pokemon et je suis en compagnie de truc";;
