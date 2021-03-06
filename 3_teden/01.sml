fun implies (z: bool * bool) =
    not (#1 z) orelse (#2 z);

fun partition (x, xs) =
let
    fun partition (xs, l, r) =
        if null xs
	then (rev l, rev r)
	else
	    if hd xs < x
	    then partition(tl xs, hd xs::l, r)
	    else partition(tl xs, l, hd xs::r)
in
    partition (xs, [], [])
end;


fun quickselect(k: int, x: int list) =
    let val p = partition(hd x + 1, x)
    in
	 if length (#1 p) = k
	 then hd (tl x)
	 else
	     if length (#1 p) > k
	     then quickselect(k, #1 p)
	     else quickselect(k - length (#1 p) - 1, #2 p)
    end;

(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n : int) =
    if n = 0
    then 1
    else n*factorial(n-1);

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x : int, n : int) =
    if n = 0
    then 1
    else x * power(x,n-1);

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, b : int) =
    if b = 0
    then a
    else gcd(b, a mod b);
    
(*  Vrne dolžino seznama. *)
fun len (xs : int list) =
    if null xs
    then 0
    else 1 + len(tl xs);

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last (xs : int list) =
    if null xs
    then NONE
    else
	if null (tl xs)
	then SOME (hd xs)
	else last(tl xs);

(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth (xs : int list, n : int) =
    if null xs
    then NONE
    else
	if n = 0
	then SOME (hd xs)
	else nth(tl xs, n-1);

(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, n : int, x : int) =
    if n = 0
    then x::xs
    else (hd xs)::insert(tl xs, n-1, x);
    
(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete (xs : int list, x : int) =
    if null xs
    then nil
    else
	if hd xs = x
	then delete(tl xs, x)
	else (hd xs)::delete(tl xs, x);

(*  Vrne obrnjen seznam. V pomoč si lahko spišete še funkcijo append, ki doda na konec seznama. *)
fun reverse (xs : int list) =
    let fun append (l: int list, el: int) =
	if null l
	then [el]
	else (hd l)::append(tl l, el)
    in
	if null xs
	then nil
	else append(reverse(tl xs), hd xs)
    end;

(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun palindrome (xs : int list) =
    xs = reverse(xs);
