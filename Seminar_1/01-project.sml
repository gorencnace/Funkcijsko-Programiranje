datatype 'a expr = !! of 'a expr
                | \/ of 'a expr * 'a expr
                | /\ of 'a expr * 'a expr
                | <=> of 'a expr * 'a expr
                | ==> of 'a expr * 'a expr
                | V of 'a
                | T | F;
infix 5 <=>;
infixr 6 ==>;
infix 7 \/;
infix 8 /\;

datatype 'a expression = Not of 'a expression
                    | Or of 'a expression list
                    | And of 'a expression list
                    | Eq of 'a expression list
                    | Imp of 'a expression * 'a expression
                    | Var of 'a
                    | True | False;

datatype 'a stream = Next of 'a * (unit -> 'a stream);
fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed) end;

fun int2bool i = LargeInt.mod (i, 2) = 1;

exception InvalidCNF;
exception NotImplemented;

Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
Control.Print.stringDepth := 1000;


fun getVars var =
    let
	fun pomozna (Var x) = [x]
	  | pomozna (Not x) = pomozna (x)
	  | pomozna (Imp (x, y)) = pomozna (x) @ pomozna (y)
	  | pomozna (Or nil) = nil
	  | pomozna (And nil) = nil
	  | pomozna (Eq nil) = nil
	  | pomozna (Or xs) = pomozna (hd xs) @ pomozna (Or (tl xs))
	  | pomozna (And xs) = pomozna (hd xs) @ pomozna (And (tl xs))
	  | pomozna (Eq xs) = pomozna (hd xs) @ pomozna (Eq (tl xs))
	  | pomozna _ = nil
	fun distinct nil = nil
	  | distinct (x::xs) = x::distinct (List.filter (fn a => a <> x) xs)
    in
	distinct (pomozna var)
    end;

fun eval spr exp =
    let
	fun vstavi (_, nil) = false
	  | vstavi (y, x::xs) = if y = x then true else vstavi (y, xs)
    in
	case exp of
	    Var x => vstavi (x, spr)
	  | Not x => not (eval spr x)
	  | Imp (x, y) => not (eval spr x) orelse eval spr y
	  | And nil => true
	  | Or nil => false
	  | Eq nil => true
	  | And (x::xs) => eval spr x andalso eval spr (And xs)
	  | Or (x::xs) => eval spr x orelse eval spr (Or xs)
	  | Eq (x::xs) => eval spr x = eval spr (Eq xs)
	  | True => true
	  | False => false
    end;

fun rmEmpty exp =
    case exp of
	And nil => True
      | Or nil => False
      | Eq nil => True
      | And (x::nil) => rmEmpty x
      | Or (x::nil) => rmEmpty x
      | Eq (x::nil) => True
      | And xs => And (map rmEmpty xs)
      | Or xs => Or (map rmEmpty xs)
      | Eq xs => Eq (map rmEmpty xs)
      | Not x => Not (rmEmpty (x))
      | Imp (x, y) => Imp (rmEmpty x, rmEmpty y)
      | x => x;

fun beautify exp =
    let
	fun pomoznaSeznam e =
	    case e of
		Eq (x1::x2::nil) => beautify x2 <=> beautify x1
	      | Eq (x1::x2::xs) => pomoznaSeznam (Eq (x2::xs)) /\ (beautify x2 <=> beautify x1)
	      | And (x1::x2::nil) => beautify x2 /\ beautify x1
	      | And (x::xs) => pomoznaSeznam (And xs) /\ beautify x
	      | Or (x1::x2::nil) => beautify x2 \/ beautify x1
	      | Or (x::xs) => pomoznaSeznam (Or xs) \/ beautify x
	fun pomozna e =
	    case e of
		Var x => V x
	      | Not x => !! (pomozna x)
	      | True => T
	      | False => F
	      | Imp (x, y) => pomozna x ==> pomozna y
	      | Eq xs => pomoznaSeznam (Eq (rev xs))
	      | And xs => pomoznaSeznam (And (rev xs))
	      | Or xs => pomoznaSeznam (Or (rev xs))
    in
	pomozna (rmEmpty exp)
    end;
	       
fun pushNegations exp =
    let
	fun pomoznaSeznamNot nil = nil
	  | pomoznaSeznamNot (x::xs) = pushNegations (Not x) :: pomoznaSeznamNot xs
	fun pomoznaSeznam nil = nil
	  | pomoznaSeznam (x::xs) = pushNegations x :: pomoznaSeznam xs
	fun pomozna e =
	    case e of
		Not (Not x) => pomozna x
	      | Not (Eq xs) => And [pomozna(Not (And xs)), pomozna (Or xs)]
	      | Not (And xs) => Or (pomoznaSeznamNot xs)
	      | Not (Or xs) => And (pomoznaSeznamNot xs)
	      | Not (Imp (x, y)) => And [pomozna x, pomozna (Not y)]
	      | Not True => False
	      | Not False => True
	      | Not x => Not (pomozna x)
	      | Or xs => Or (pomoznaSeznam xs)
	      | And xs => And (pomoznaSeznam xs)
	      | Eq xs => Eq (pomoznaSeznam xs)
	      | Imp (x, y) => Imp (pomozna x, pomozna y)
	      | x => x
    in
	pomozna (rmEmpty exp)
    end;

fun rmConstants exp =
    let
	fun rmc	(Not (Var x)) = Not (Var x)
	  | rmc (Not x) = rmConstants (pushNegations (Not x))
	  | rmc (Imp (x, y)) =
	    (case (rmc x, rmc y) of
		 (a, False) => pushNegations (Not a)
	       | (_, True) => True
	       | (False, _) => True
	       | (True, a) => a
	       | (a, b) => Imp (a, b))
	  | rmc (And xs) = (let
			       val sez = List.filter (fn x => rmc x <> True) xs
			   in
			       if List.exists (fn x => rmc x = False) sez
			       then False
			       else And (List.map rmc sez)
			   end)
	  | rmc (Or xs) = (let
			      val sez = List.filter (fn x => rmc x <> False) xs
			  in
			      if List.exists (fn x => rmc x = True) sez
			      then True
			      else Or (List.map rmc sez)
			  end)
	  | rmc (Eq [x, y]) =
	    (case (rmc x, rmc y) of
		 (a, False) => rmc (Not a)
	       | (a, True) => rmc a
	       | (False, a) => rmc (Not a)
	       | (True, a) => rmc a
	       | (a, b) => Eq [a, b])
	  | rmc (Eq xs) = rmConstants (Or [And xs, pushNegations (Not (Or xs))])
	  | rmc x = x
    in
	rmEmpty (rmc (rmEmpty exp))
    end;

fun rmVars exp =
    let
	fun rmv (Not x) = Not (rmv x)
	  | rmv (Imp (x, y)) = if rmv x = rmv y then True else rmEmpty (Imp (rmv x, rmv y))
	  | rmv (And xs) = rmEmpty (And (List.foldl (fn (y, ys) => if List.exists (fn x => rmv x = rmv y) ys then ys else ys @ [rmv y]) nil xs))
	  | rmv (Or xs) = rmEmpty (Or (List.foldl (fn (y, ys) => if List.exists (fn x => rmv x = rmv y) ys then ys else ys @ [rmv y]) nil xs))
	  | rmv (Eq [x, y]) = if rmv x = rmv y then True else rmEmpty (Eq [rmv x, rmv y])
	  | rmv (Eq xs) = rmEmpty (Eq (List.foldl (fn (y, ys) => if List.exists (fn x => rmv x = rmv y) ys then ys else ys @ [rmv y]) nil xs))
	  | rmv x = x
    in
	rmv (rmEmpty exp)
    end;

fun simplify exp =
    let
	val a = (rmVars (pushNegations (rmConstants exp)))
    in
	if a = exp then exp else simplify(a)
    end;

fun prTestEq seed exp1 exp2 = true;

fun toWolframLang toString exp =
    case exp of
	Not x => "Not[" ^ (toWolframLang toString x) ^ "]"
      | True => "True"
      | False => "False"
      | Var x => "Var[\"" ^ (toString x) ^ "\"]"
      | Imp (x, y) => "Implies[" ^ (toWolframLang toString x) ^ ", " ^ (toWolframLang toString y) ^ "]"
      | Eq xs => "Equivalent[" ^ (String.concatWith ", " (List.map (fn x => toWolframLang toString x) xs)) ^ "]"
      | And xs => "And[" ^ (String.concatWith ", " (List.map (fn x => toWolframLang toString x) xs)) ^ "]"
      | Or xs => "Or[" ^ (String.concatWith ", " (List.map (fn x => toWolframLang toString x) xs)) ^ "]";
