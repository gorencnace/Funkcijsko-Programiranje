val a = 3;

fun obseg (r: real) = 2.0 * Math.pi * r;

fun potenca (x: int, y: int) =
    if y = 0 then 1 else x * potenca(x,y-1);

fun faktoriela (n: int) =
    if n=0 then 1 else n * faktoriela(n-1);

fun vsotaAB (a: int, b: int) =
    if a = b then b else b + vsota(a, b-1);

fun vsota1N (n: int) =
    vsota(1,n);
