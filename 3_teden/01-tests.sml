val _ = print "---------- factorial ----------";
val _ : int -> int = factorial;
val test1_1 = factorial 0 = 1;
val test1_2 = factorial 1 = 1;
val test1_3 = factorial 5 = 120;


val _ = print "---------- power ----------";
val _ : int * int -> int = power;
val test2_1 = power(0, 0) = 1;
val test2_2 = power(0, 2) = 0;
val test2_3 = power(2, 10) = 1024;


val _ = print "---------- gcd ----------";
val _ : int * int -> int = gcd;
val test3_1 = gcd(3, 7) = 1; 
val test3_2 = gcd(14, 4) = 2;
val test3_3 = gcd(18, 12) = 6;


val _ = print "---------- len  ----------";
val _ : int list -> int = len;
val test4_1 = len([]) = 0;
val test4_2 = len([1, 2, 3]) = 3;
val test4_3 = len([2]) = 1;


val _ = print "---------- last ----------";
val _ : int list -> int option = last;
val test5_1 = last([]) = NONE;
val test5_2 = last([1,3,4,5,6,8]) = SOME 8;
val test5_3 = last([3]) = SOME 3;


val _ = print "---------- nth ----------";
val _ : int list * int -> int option = nth;
val test6_1 = nth([], 3) = NONE;
val test6_2 = nth([1,2,3,4,5,6,7], 4) = SOME 5;
val test6_3 = nth([1,2,3,4], 4) = NONE;
val test6_4 = nth([1], 0) = SOME 1;


val _ = print "---------- insert ----------";
val _ : int list * int * int -> int list = insert;
val test7_1 = insert([], 0, 1) = [1];
val test7_2 = insert([1, 2, 3], 1, 4) = [1, 4, 2, 3];
val test7_3 = insert([1, 2, 3], 3, 4) = [1, 2, 3, 4];


val _ = print "---------- delete ----------";
val _ : int list * int -> int list = delete;
val test8_1 = delete([], 1) = [];
val test8_2 = delete([1, 1, 1, 1], 1) = [];
val test8_3 = delete([1, 2, 3, 4, 5, 6, 7], 8) = [1, 2, 3, 4, 5, 6, 7];
val test8_4 = delete([1, 2, 3, 4, 5, 6], 3) = [1, 2, 4, 5, 6];
val test8_5 = delete([1, 2, 3, 1, 2, 1, 1, 3, 2, 1], 1) = [2, 3, 2, 3, 2];


val _ = print "---------- reverse ----------";
val _ : int list -> int list = reverse;
val test9_1 = reverse([1, 2, 3, 4, 5]) = [5, 4, 3, 2, 1];
val test9_2 = reverse([]) = [];
val test9_3 = reverse([3]) = [3];


val _ = print "---------- palindrome ----------";
val _ : int list -> bool = palindrome;
val test10_1 = palindrome([]) = true;
val test10_2 = palindrome([1]) = true;
val test10_3 = palindrome([1, 2, 3, 2, 1]) = true;
val test10_4 = palindrome([1, 2, 3, 3, 2, 1]) = true;
val test10_5 = palindrome([1, 2, 3, 4, 5]) = false;
