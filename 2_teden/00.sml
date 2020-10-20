(* Vrne naslednika stevila n. *)
fun next (n : int) = n+1;

(* Vrne vsoto stevil a in b. *)
fun add (a : int, b : int) = a+b;

(* Vrne true, ce sta vsaj dva argumenta true, drugace vrne false *)
fun majority (a : bool, b : bool, c : bool) = (a andalso b) orelse (a andalso c) orelse (b andalso c);

(* Vrne mediano argumentov - stevila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real) *)
fun median (a : real, b : real, c : real) = Real.max(Real.min(a,b), Real.max(Real.min(a,c), Real.min(b,c)));

(* Preveri ali so argumenti veljavne dolzine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a : int, b : int, c : int) : bool = (a+b-c > 0) andalso (a-b+c > 0) andalso (~a+b+c > 0);
