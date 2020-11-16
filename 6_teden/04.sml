fun f x y = x * y;

fun curry f x y = f (x,y);

fun uncurry f (x, y) = f x y;

fun swap f x y = f y x;

val compose = op o;
infix 3 o;
fun compose (f, g) = f o g;

fun compose2 f g = f o g;

fun apply (f, x) = f x;

fun apply2 f x = f x;

fun foldl f z nil = z
  | foldl f z x = foldl f (f (z, hd x)) (tl x);

fun foldl2 f z nil = z
  | foldl2 f z x = foldl2 f (f z (hd x)) (tl x);

fun fildr f z nil = z
  | fildr f z x = f (foldr z (tl x), hd x);

fun foldr2 f z nil = z
  | foldr2 f z x = f (foldr2 f z (tl x)) (hd x);

fun map f nil = nil
  | map f xs = f (hd xs) :: map f (tl xs);

fun iterate 0 f = (fn x => x)
  | iterate n f = f o (iterate (n-1) f);

fun D f =
    fn x =>
       let val h = 1E~5
       in (f (x+h) - f (x)) / h
       end;

fun Dn n = iterate n D;


(* Naloge za oddajo *)
fun reduce _ z nil = z
  | reduce f z (x::xs) = reduce f (f z x) xs;

fun squares nil = nil
  | squares (x::xs) = (x*x)::squares xs;

fun onlyEven xs = List.filter (fn x => if x mod 2 = 0 then true else false) xs;

fun bestString f nil = ""
  | bestString f (x::xs) = List.foldl (fn (a, b) => if f (a, b) then a else b) x xs;

fun largestString xs =
    bestString
	(fn (x, y) =>
	       case String.compare(x, y) of
		   LESS => false
		 | GREATER => true
		 | _ => true)
	xs;

fun longestString xs =
    bestString (fn (x, y) => String.size x >= String.size y) xs;

fun quicksort f (x::xs) =
    let
	val (a, b) = List.partition (fn a => f (a, x) = LESS) xs
    in
	quicksort f a @ quicksort f b
    end
  | quicksort _ x = x
    

fun dot xs ys = List.foldl (fn (x,y) => x+y) 0 (ListPair.map (fn (x,y) => x*y) (xs, ys));

fun transpose m =
    let
	val r = length m
	val c = if r > 0
		then length (hd m)
		else 0
    in
	List.tabulate (c, (fn x => List.map (fn row => (List.nth (row, x))) m))
    end;

fun multiply m1 m2 =
    let
	val m2t = transpose m2
	val c = length m2t
	val r = length m1
    in
	List.tabulate (r, (fn x => List.tabulate (c, (fn y => dot (List.nth (m1, x)) (List.nth (m2t, y))))))
    end;

fun group nil = nil
  | group xs =
    let
	val sez = ref [(hd xs, 1)]
	val i = ref 1
	val item = ref (hd (!sez))
    in
	while !i < List.length xs do (
	    item := (List.last (!sez));
	    if #1 (!item) = List.nth (xs, !i)
	    then sez := (List.take (!sez, List.length (!sez) - 1)) @ [(#1 (!item), #2 (!item) + 1)]
	    else sez := !sez @ [(List.nth (xs, !i), 1)];
	    i := !i + 1
	);
	!sez
    end;

fun equivalenceClasses _ nil = nil
  | equivalenceClasses f xs =
    let
	val sez = ref nil
	val new_xs = ref xs
	val x = ref (nil, nil)
    in
	while not (null (!new_xs)) do (
	    x := List.partition (fn y => f y (hd (!new_xs))) (!new_xs);
	    sez := !sez @ [(#1 (!x))];
	    new_xs := #2 (!x)
	);
	!sez
    end;
