fun vsota (stevili: int*int) = (#1 stevili) + (#2 stevili);

fun obrni (stevili: int*int) = (#2 stevili, #1 stevili);

fun sort (t: int*int) =
    if #1 t < #2 t
    then t
    else obrni(t);

fun list_len (l: int list) =
    if null l
    then 0
    else 1 + st_elementov(tl l);

fun list_sum (l: int list) =
    if null l
    then 0
    else hd l + list_sum(tl l);

fun list_n_el (l: int list, n: int) =
    if n = 1
    then hd l
    else list_n_el(tl l, n-1);

fun list_conc (l1: int list, l2: int list) =
    if null l1
    then  l2
    else hd l1 :: list_conc(tl l1, l2);

fun list_prepleti (l1: int list, l2: int list) =
    if null l1 orelse null l2
    then []
    else (hd l1, hd l2) :: list_prepleti(tl l1, tl l2);

fun list_vsote_terk (l: (int*int) list) =
    if null l
    then []
    else (#1 (hd l) + #2( hd l)) :: list_vsote_terk(tl l);

fun list_poz_predmeti (l: (string*int) list) =
    if null l
    then []
    else
	if #2 (hd l) > 5
	then #1 (hd l) :: list_poz_predmeti(tl l)
	else list_poz_predmeti(tl l);

fun list_max (l: int list) =
    if null l
    then 0
    else
	if null (tl l)
	then hd l
	else
	    if hd l > list_max(tl l)
	    then hd l
	    else list_max(tl l);

