structure Rational =
struct

datatype rational = Stevilo of int | Ulomek of int * int

exception BadRational

fun makeRational (_, 0) = raise BadRational
  | makeRational (a, 1) = Stevilo a
  | makeRational (1, b) = if b < 0 then Ulomek (~1, ~b) else Ulomek (1, b)
  | makeRational (a, b) =
    if a mod b = 0
    then Stevilo (a div b)
    else
	let
	    val i = ref 2
	    val x = ref a
	    val y = ref b
	in
	    while ((fn (x, y) => if x > y then x else y) (!x, !y) > !i) do (
		if (!x) mod (!i) = 0 andalso (!y) mod (!i) = 0
		then
		    (x := (!x) div (!i);
		    y := (!y) div (!i))
		else
		    i := (!i) + 1
	    );
	    if !y < 0 then Ulomek (~(!x), ~(!y)) else Ulomek (!x, !y)
	end

fun neg (Stevilo a) = Stevilo (~a)
  | neg (Ulomek (a, b)) = Ulomek (~a, b)

fun inv (Stevilo a) = makeRational (1, a)
  | inv (Ulomek (a, b)) = makeRational (b, a)

fun add (Stevilo a, Stevilo b) = Stevilo (a + b)
  | add (Stevilo a, Ulomek (b, c)) = makeRational (a * c + b, c)
  | add (Ulomek (b, c), Stevilo a) = makeRational (a * c + b, c)
  | add (Ulomek (a, b), Ulomek (c, d)) = makeRational (a*d + b*c, b*d)


fun mul (Stevilo a, Stevilo b) = Stevilo (a * b)
  | mul (Stevilo a, Ulomek (b, c)) = makeRational (a * b, c)
  | mul (Ulomek (b, c), Stevilo a) = makeRational (a * b, c)
  | mul (Ulomek (a, b), Ulomek (c, d)) = makeRational (a*c, b*d)
	
fun toString (Stevilo a) = Int.toString(a)
  | toString (Ulomek (a, b)) = Int.toString(a) ^ "/" ^ Int.toString(b)
end;


signature EQ =
sig
    type t
    val eq : t -> t -> bool
end;

signature SET =
sig
    (* podatkovni tip za elemente mnozice *)
    type item

    (* podatkovni tip mnozico *)
    type set

    (* prazna mnozica *)
    val empty : set

    (* vrne mnozico s samo podanim elementom *)
    val singleton : item -> set

    (* unija mnozic *)
    val union : set -> set -> set

    (* razlika mnozic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva mnozica podmnozica druge *)
    val subset : set -> set -> bool
end;

functor SetFn (Eq : EQ) :> SET where type item = Eq.t =
struct
type item = Eq.t
type set = item list
val empty : set = []
fun singleton i = [i]
fun union s1 s2 =
    let fun dist nil = nil
	  | dist (x::xs) = x::dist (List.filter (fn y => not (Eq.eq x y)) xs)
    in
	dist (s1 @ s2)
    end
fun difference xs nil = xs
  | difference xs (y::ys) =  difference (List.filter (fn x => not (Eq.eq x y)) xs) ys
fun subset nil _ = true
  | subset (x::xs) ys = List.exists (fn y => Eq.eq x y) ys andalso subset xs ys
end;

funsig SETFN (Eq : EQ) = SET;
