(*** LASTNI PODATKOVNI TIPI ***)

(** Ujemanje vzorcev **)
datatype yesNo = Yes | No;

fun toString1 x =
    case x of
	Yes => "Da"
      | No => "Ne";

fun toString2 Yes = "Da" 
  | toString2 No = "Ne";

(** Kvartopirska **)

datatype barva = Kriz | Pik | Srce | Karo
					 
datatype stopnja = As | Kralj | Kraljica | Fant | Stevilka of int

type karta = stopnja * barva;

val karte1 = [(Stevilka 2,Pik), (Stevilka 4,Pik), (As,Pik), (Fant,Pik),(Stevilka 8,Pik)];

val karte2 = [(Stevilka 2,Pik), (Stevilka 5,Pik), (As,Karo), (Fant,Pik), (Stevilka 8,Pik)];
	   
(* Kaksne barve je karta? *)
fun barvaKarte ((_,b) : karta) = b;

(* Ali je karta veljavna? *)
fun veljavnaKarta ((st,_) : karta) =
    case st of
	Stevilka i => if i >= 2 andalso i <= 10 then true else false
      | _ => true;

(* Koliko je vredna karta? *)
fun vrednostKarte ((st,_) : karta) =
    case st of
	Stevilka i => i
      | _ => 10;

(* Koliksna je vrednost vseh kart v roki? *)  
fun vsotaKart (ks : karta list) =
    if null ks
    then 0
    else vrednostKarte(hd ks) + vsotaKart(tl ks);

(* Ali imam v roki karte iste barve? *)
fun isteBarve (ks : karta list) =
    if null ks
    then true
    else let fun barva(b: barva, kl: karta list) =
		 if null (tl kl)
		 then barvaKarte(hd kl) = b
		 else barvaKarte(hd kl) = b andalso barva(b,tl kl)
	 in barva(barvaKarte(hd ks), tl ks)
	 end;

(** Cela stevila **)
datatype number = Zero
		| Succ of number
		| Pred of number;

(* Funkcija pretvori stevilo v najbolj ekonomicno *)
fun simp (n: number) =
    let
	fun prestej(p: int, nu: number) =
	    case nu of
		Zero => p
	      | Succ x => prestej(p+1, x)
	      | Pred x => prestej(p-1, x)
	fun zgradi(p: int, num: number) =
	    if p = 0
	    then num
	    else
		if p < 0
		then zgradi(p+1, Pred(num))
		else zgradi(p-1, Succ(num))
    in
	zgradi(prestej(0, n), Zero)
    end;

fun simp1 Zero = Zero
  | simp1 (Pred (n)) = let val a = simp1 (n)
		       in
			   case a of
			       Succ(x) => x
			     | _ => Pred (a)
		       end
  | simp1 (Succ (n)) = let val a = simp1 (n)
		       in
			   case a of
			       Pred (x) => x
			     | _ => Succ(a)
		       end;

(** Drevesa **)
datatype tree = Node of int * tree * tree
	      | Leaf of int;

val drevo1 =
    Node (5,
	  Node (3, Leaf 2, Leaf 4),
	  Node (4, Leaf 1, Leaf 3)
	 );

val drevo2 =
    Node (0,
	  Node (~2, Leaf ~3, Leaf ~1),
	  Node (2, Leaf 1, Leaf 3)
	 );

val drevo3 =
    Node (1,
	  Node (3,
		Node (2, Leaf 3, Leaf 4),
		Leaf 0),
	  Leaf 2
	 );

(* Vrni najmanjsi element v drevesu *)
fun min (t: tree) =
    let fun minInt(a: int, b: int) =
	    if a < b
	    then a
	    else b
    in
	case t of
	    Leaf x => x
	  | Node (x, L, R) => minInt(minInt(min(L), min(R)), x)
    end;

(* Vrni najvecji element v drevesu *)
fun max (t: tree) =
    let fun maxInt (a: int, b: int) =
	    if a > b
	    then a
	    else b
    in
	case t of
	    Leaf x => x
	  | Node (x, L, R) => maxInt(maxInt(max(L), max(R)), x)
    end;

(** Naloge za oddajo **)

(* Negira stevilo a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) =
    case a of
	Succ x => Pred (neg (x))
      | Pred x => Succ (neg (x))
      | Zero => Zero;

(* Vrne vsoto stevil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) =
    case a of
	Zero => b
      | Pred x => Pred (add (x,b))
      | Succ x => Succ (add (x,b));

(* Vrne rezultat primerjave stevil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) =
    let val sa = simp a
	val sb = simp b
    in
	case (sa, sb) of
	    (Zero, Zero) => EQUAL
	  | (Succ xa, Succ xb) => comp (xa, xb)
	  | (Pred xa, Pred xb) => comp (xa, xb)
	  | (Succ xa, _) => GREATER
	  | (_, Pred xb) => GREATER
	  | _ => LESS
    end;
				

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) =
    case tree of
	Leaf l => l = x
      | Node (v, L, R) => v = x orelse contains (L, x) orelse contains (R, x);

(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) =
    case tree of
	Node (_, L, R) => 1 + countLeaves(L) + countLeaves(R)
      | _ => 1;

(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) =
    case tree of
	Node (_, L, R) => 2 + countBranches(L) + countBranches(R)
      | _ => 0;

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) =
    case tree of
	Node (_, L, R) => 1 + Int.max (height (L), height (R))
      | _ => 1;

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree : tree) =
    case tree of
	Leaf x => [x]
      | Node (x, L, R) => toList(L) @ [x] @ toList (R);

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree : tree) =
    case tree of
	Leaf _ => true
      | Node (_, L, R) => Int.abs (height (L) - height (R)) <= 1 andalso isBalanced (L) andalso isBalanced (R);

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree : tree) =
    case tree of
	Leaf _ => true
      | Node (x, L, R) => max (L) < x andalso x < min (R) andalso isBST (L) andalso isBST (R);

(** Dodatne naloge **)
datatype bstree = br of bstree * int * bstree | lf;
type name = {first : string, last : string};
type date = {month : string, day : int, year : int};
type person = {name : name, birthdate : date};
