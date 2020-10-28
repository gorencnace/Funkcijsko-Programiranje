datatype izraz = Konstanta of int
	       | Negiraj of izraz
	       | Plus of izraz * izraz
	       | Minus of izraz * izraz
	       | Krat of izraz * izraz
               | Deljeno of izraz * izraz
	       | Ostanek of izraz * izraz;

val izraz1 = Konstanta 3;
val izraz2 = Negiraj (Konstanta 3);
val izraz3 = Plus (Konstanta 3, Ostanek (Konstanta 18, Konstanta 4));
val izraz4 = Deljeno (izraz3, Negiraj izraz2);
				
fun eval e =
    case e of
        Konstanta i => i
      | Negiraj e  =>  ~ (eval e)
      | Plus (e1, e2) => (eval e1) + (eval e2)
      | Minus (e1, e2) => (eval e1) - (eval e2)
      | Krat (e1, e2) => (eval e1) * (eval e2)
      | Deljeno (e1, e2) => (eval e1) div (eval e2) 				      | Ostanek (e1, e2) => (eval e1) mod (eval e2);

(* dewfinicija lastnefa seznama *)
datatype mojlist = konec
		 | Sez of int * mojlist;

datatype intopcija =
	 SOM of int
       | NON;

exception Prazen;

(*
fun glava sez =
    case sez of
	Sez (i, ostanek) => i
     |  konec => raise Prazen;
*)
(* seznam, ki hrani elemente dveh različnih podatkovnih tipov *)
datatype ('a, 'b) seznam =
	 Elementa of ('a * ('a, 'b) seznam)
       | Elementb of ('b * ('a, 'b) seznam)
       | konec;

(* fn: ('a, 'b) seznam -> (int * int) *)
fun prestej sez =
    case sez of
	Elementa(x, preostanek) => let val vp = prestej (preostanek)
				    in (1 + (#1 vp), #2 vp)
				   end
     | Elementb(x, preostanek) => let val vp = prestej (preostanek)
				    in (#1 vp, 1 + (#2 vp))
				    end
     |  konec => (0,0);


fun sestej1 (trojcek: int * int * int) =
    let val (a, b, c) = trojcek
    in a+b+c
    end;
fun sestej2 (a, b, c) = a+b+c;
fun povecaj (a, b, c) = (a+1, b+1, c+1);

fun dolzina (sez: int list) =
    case sez of
	nil => 0
      | _::rep => 1 + (dolzina rep);


(* rekurzivno ujemanje vzorcev *)

exception LengthProblem

(* sestej dva seznama, ki morata biti iste dolzine *)
fun sestej_seznam(sez1, sez2) =
    case sez1 of
	[] => (case sez2 of
		   [] => []
		|  glava::rep => raise LengthProblem)
      | glava1::rep1 => (case sez2 of
			     [] => raise LengthProblem
			   | glava2::rep2 => (glava1 + glava2)::sestej_seznam(rep1, rep2));

(* -> REKURZIVNO gnezdenje vzorcev *)
fun sestej_seznam2 seznama =
    case seznama of
	([],[]) => []
      | (g1::r1, g2::r2) => (g1+g2)::sestej_seznam2(r1, r2)
      | _ => raise LengthProblem; 

fun check_fibonacci sez =
    case sez of
	glava::drugi::tretji::rep => (tretji = (drugi + glava)) andalso check_fibonacci (drugi::tretji::rep)
      | _ => true;


(* funkcija, ki ipise sodost/lihost dveh stevil a in b po njumem sestevanju *)
datatype sodost = S | L | N;
fun sodost_sestevanje (a,b) =
    let
	fun sodost x = if x = 0
		       then N
		       else if x mod 2 = 0
		       then s
		       else L
    in
	case (sodost a, sodost b) of
	    (S,L) => L
	 |  (S,_) => S
	 |  (L,L) => S
	 |  (L,_) => L
	 |  (N,x) => x
    end;
