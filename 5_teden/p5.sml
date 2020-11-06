fun sestej1 (stevili: int * int * int) =
    #1 stevili + #2 stevili;

fun sestej3 (s1, s2) =
    s1 + s2;

fun vsota_el sez =
    case sez of
    [] => 0
  | g::r => g + vsota_el r;

fun zdruzi (s1, s2) =
    case s1 of
	[] => s2 
      | g::r => g::zdruzi(r, s2);

fun sestej_zapis {prvi=a, drugi=b, tretji=c, cetrti=d, preti=e} = a+d;

fun f1 (a,b,c,d) =
    if a=b
    then c
    else d;
