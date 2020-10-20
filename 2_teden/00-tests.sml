val _ = print "--------- next ---------";
val _ : int -> int = next;
val test1_1 = next 1 = 2;
val test1_2 = next 3 = 4;
val test1_3 = next 5 = 6;

val _ = print "--------- add ---------";
val _ : int * int -> int = add;
val test2_1 = add(1,3) = 4;
val test2_2 = add(~4,~5) = ~9;
val test2_3 = add(5,~7) = ~2;

val _ = print "--------- majority ---------";
val _ : bool * bool * bool -> bool = majority;
val test3_1 = majority(true, false, false) = false;
val test3_2 = majority(false, true, true) = true;
val test3_3 = majority(true, false, true) = true;

val _ = print "--------- median ---------";
val _ : real * real * real -> real = median;
val test4_1 = Real.==(median(1.3,4.7,7.9), 4.7);
val test4_2 = Real.==(median(6.2,~1.3,0.1), 0.1);
val test4_3 = Real.==(median(3.4,10.3,~2.3), 3.4);

val _ = print "--------- triangle ---------";
val _ : int * int * int -> bool = triangle;
val test5_1 = triangle(2,3,4) = true;
val test5_2 = triangle(10,20,5) = false;
val test5_3 = triangle(2,1,3) = false;
