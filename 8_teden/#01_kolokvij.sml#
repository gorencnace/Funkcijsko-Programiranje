fun f1 (a,b,c::d) [i,j] =
 if c
 then fn a => b (SOME i)
 else fn b => a (j+1);

datatype ('a, 'b) chain = Node of {a:'a ref, b:'b} * ('b, 'a) chain | final;

fun chain_to_list final = (nil, nil)
  | chain_to_list (Node (d, next)) =
    let val (x, y) = chain_to_list next
    in ((!(#a d))::y, #b d::x)
    end;

val izraz = Node({a=ref 15, b="pon"},
 Node ({a=ref "tor", b = 12},
 Node ({a=ref 42, b="sre"},
 Node ({a=ref "cet", b=314},
       final))));

signature KarteP =
sig
    type karta
    type karte
    type barva
    val dodaj_v_roke : karta -> karte
    val nova_karta : barva * int -> karta
    val pokazi_roke: unit -> karte
end;

structure Karte :> KarteP =
struct
datatype barva = Pik | Karo | Srce | Kriz
datatype karta = Karta of (barva * int) | Joker
type karte = string list
val v_roki = ref []:karte ref
exception NeveljavnaKarta of (barva * int)
fun nova_karta (barva, int) =
 if (int>=2 andalso int <=14) then Karta(barva,int) else raise NeveljavnaKarta(barva,int)
fun dodaj_v_roke (nova:karta) =
 let val count_jokers = List.foldl (fn (el,ac)=>if (el="Joker") then ac+1 else ac) 0 (!v_roki)
 in
 (case nova of
 Joker => if (count_jokers <4) then v_roki := (!v_roki) @ ["Joker"] else ()
| _ => v_roki := (!v_roki) @ ["Karta"]
 ; (!v_roki))
 end
fun pokazi_roke () = (!v_roki)
end;

datatype pot = Left of pot | Right of pot
	       | Up of pot | Down of pot | start;

fun coordinate smer =
    let
	fun pomozna (smer, a, b) =
	    case smer of
		start => {x=a, y=b}
	      | Left x => pomozna (x, a-1, b)
	      | Right x => pomozna (x, a+1, b)
	      | Down x => pomozna (x, a, b-1)
	      | Up x => pomozna (x, a, b+1)
    in pomozna (smer, 0, 0) end;

fun f (a, b) (c::d) e =
    if (!b) = (c mod 2 = 0)
    then fn x => e
    else fn (y,z) => valOf (z) andalso a mod 2 = 1;


datatype vrtnarstvo = Obrezi of {ime:string, cas:int}
		    | Zalij of {ime:string, kolicina:int}
		    | Obcuduj of {ime:string, cas:int}
		    | Meditiraj of {cas:int}
		    | Vonjaj of {ime:string};
