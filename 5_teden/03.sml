(* Polimorfizem in konstruktorji tipov *)
datatype ('prvi, 'drugi) seznamParov = Prazen | Element of 'prvi * 'drugi * ('prvi, 'drugi) seznamParov;  
type 'a multiMnozica = ('a, int) seznamParov;

(* Sprogramiranj funkcijo seznamParov: 'a list * 'b list -> ('a,'b) seznamParov *)
fun seznamParov (hda::tla, hdb::tlb) = (Element (hda, hdb, seznamParov(tla, tlb))) 
  | seznamParov _ = Prazen;


fun foldl_n (f, z, s) =
    case s of
	hs::nil => f (z, hs)
      | hs::ts => foldl_n (f, f (z, hs), ts);

fun foldr_n (f, z, s) =
    case s of
	nil => z
      | hs::ts => f (foldr_n (f, z, ts), hs);

fun append (al, bl) =
    let fun pomozna (x, y) = x::y
    in foldr pomozna bl al
    end;

(* Naloge za oddajo *)
datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip (ha::ta, hb::tb) = (ha, hb)::zip(ta, tb)
  | zip (_, _) = nil;

fun unzip ((a, b)::t) =
    let val (ta, tb) = unzip (t)
    in
	(a::ta, b::tb)
    end 
  | unzip _ = (nil, nil);

fun subtract (One, _) = raise NotNaturalNumber
  | subtract (Succ x, One) = x
  | subtract (Succ x, Succ y) = subtract (x, y);

fun any (f, nil) = false
  | any (f, h::t) =
    if f (h)
    then true
    else any (f, t);

fun map (f, h::t) = f (h)::map (f, t)
  | map (_, _) = nil;

fun filter (f, s) = foldr (fn (x, y) => if f (x) then x::y else y) nil s;

fun fold (f, z, s) =
    case s of
	nil => z
      | hs::ts => fold (f, f (z, hs), ts);

val avl_tree =
    br (
	br (
	    br (lf, 1, lf),
	    2,
	    br (lf, 3, lf)
	),
	4,
	br (
	    br (lf, 5, lf),
	    6,
	    br (lf, 7, lf)
	)
    );


fun rotate (br (br (a, p, b), q, c), R) = br (a, p, br (b, q, c))
  | rotate (br (a, p, br (b, q, c)), L) = br (br (a, p, b), q, c)
  | rotate (tree, _) = tree;


fun height lf = 0
  | height (br (l, v, r)) = 1 + Int.max (height (l), height (r));

fun factor lf = 0
  | factor (br (l, v, r)) = height (r) - height (l);

fun rebalance lf = lf
  | rebalance (br (a, b, c)) =
    case factor (br (a, b, c)) of
	~2 => (case factor (a) of
		   ~1 => rotate (br (a, b, c), R)
		  | _ => rotate (br (rotate (a, L), b, c), R)
	      )
       | 2 => (case factor (c) of
		    1 => rotate (br (a, b, c), L)
		  | _ => rotate (br (a, b, rotate (c, R)), L)
	      )
       | _ => br (a, b, c)  


fun avl (c, lf, e) = br (lf, e, lf)
  | avl (c, br (l, v, r), e) =
    case c (e, v) of
	LESS => rebalance (br ( avl (c, l, e), v, r))
      | GREATER => rebalance (br (l, v, avl (c, r, e)))
      | _ => br (l, v, r); 
    
				  

(* izpis daljsih izrazov v interpreterju *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

(* izpis drevesa po nivojih *)
fun showTree (toString : 'a -> string, t : 'a bstree) =
let fun strign_of_avltree_level (lvl, t) = case t of  
        lf => if lvl = 0 then "nil" else "   "
    |   br (l, n, r) =>
        let val make_space = String.map (fn _ => #" ")
            val sn = toString n
            val sl = strign_of_avltree_level (lvl, l)
            val sr = strign_of_avltree_level (lvl, r)
        in if height t = lvl
            then make_space sl ^ sn ^ make_space sr
            else sl ^ make_space sn ^ sr
        end
    fun print_levels lvl =
        if lvl >= 0
        then (print (Int.toString lvl ^ ": " ^ strign_of_avltree_level (lvl, t) ^ "\n");
                    print_levels (lvl - 1))
        else ()
  in  print_levels (height t)
end;

(* primeri vstavljanja elementov v AVL drevo *)
fun avlInt (t, i) = avl (Int.compare, t, i);
fun showTreeInt t = showTree(Int.toString, t);

val tr = lf : int bstree;
val _ = showTreeInt tr;
val tr = avlInt (tr, 1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 2);
val _ = showTreeInt tr;
val tr = avlInt (tr, 3);
val _ = showTreeInt tr;
val tr = avlInt (tr, 4);
val _ = showTreeInt tr;
val tr = avlInt (tr, 5);
val _ = showTreeInt tr;
val tr = avlInt (tr, 6);
val _ = showTreeInt tr;
val tr = avlInt (tr, 7);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~4);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~3);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~2);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 0);
val _ = showTreeInt tr;

val from0to13 = fold (fn (z, x) => avl (Int.compare, z, x), lf, List.tabulate (14, fn i => i));
