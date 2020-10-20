(* najvecji3: int list -> int option *)
fun najvecji3 (l: int list) =
    if null l
    then NONE
    else let val max_rep = najvecji3(tl l)
	 in
	     (* short circuit logic / kratkosticna logika *)
	     if isSome(max_rep) andalso hd l > valOf(max_rep)
	     then SOME (hd l)
	     else max_rep
	 end;

(* poisci prvo lokacijo pojavitve elementa el *)
fun najdi(l: int list, el: int) =
    if null l
    then NONE
    else if (hd l = el)
    then SOME 1
    else let val preostanek = najdi(tl l, el)
	 in
	     if isSome preostanek
	     then SOME (1 + valOf preostanek)
	     else NONE
	 end;


type student = {absolvent: bool, ime: string, ocene: (string * int) list, starost: int};


fun izpis_student (zapis: student) =
    (#ime zapis) ^ " je star " ^ Int.toString(#starost zapis) ^" let.";

type artikel = string * int;

val a1 = ("kruh", 1);

datatype prevozno_sredstvo = Bus of int
			   | Avto of string * string
                           | Pes;
		 
				

fun obdelaj_prevoz (x: prevozno_sredstvo) =
    case x of
	Bus i => i+10
     |  Avto (s1,s2) => String.size s1 + String.size s2
     |  Pes  => 0;

exception Prazen
fun glava (sez)=
    case sez of
	nil => raise Prazen
      | prvi::ostali => prvi;
