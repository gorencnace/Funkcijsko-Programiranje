use "01-project.sml";
val _ = print "\n\n========== running private tests... ==========\n";

val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;
val _ = Control.polyEqWarn := false;


datatype 'a stream = Next of 'a * (unit -> 'a stream);
fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed)
    end;

fun int2bool i = LargeInt.mod (i, 2) = 1;


(* WIP tests ! *)

val all_tests : bool list ref = ref [];



(* ==================== PART 1 ==================== *)



val exp1 = And [Var 1, Not (Var 2), Or [Var 3, False, Not (Var 4)]];
val exp2 = Or [Var 1, Not (Var 2), Or [Var 3, Var 1, Not (Var 4)]];
val exp3 =  And [Var 1, Not (Var 2), And [Var 3, True, Not (Var 4)]];
val exp4 =  And [Var 1, Not (False), Eq [Var 3, Var 1, Not (Var 4)]];
val exp5 = Eq [Var 1, True, Not (Var 3), Or [Var 1, False, Var 5], Imp (And [True, Not (Var 1)], False)];
val exp6 =  And [Var 1, Not (False), Or[], And[Or[Eq[And[Var 1], Eq []]]], Eq [Var 3, Var 1, Not (Var 4)]];
val exp7 = Eq [Var 1, True, Not (Var 3), Or [Var 1], Imp (And [True, Not (Var 1), Eq []], False)];
val exp8 =  And [Var 1, Var 1, Eq [Var 1, Var 1, Imp (Var 2, Var 2)]];
val exp9 = Eq [Var 1, True, Var 1, Or [Var 1, False, Var 1], Imp (And [Var 2, Var 2], False)];
val exp10 = Or [Var 1, Not (Var 1), Var 3];
val exp11 = And [Or[], Var 1, Not(Var 3), Or [Var 2, Not (Var 1), Var 3]];



val _ = print "---------- getVars ----------\n";
val _ : ''a expression -> ''a list = getVars;
val test1 = getVars (And [Var "A", Var "D", Imp (Var "X", Not (Var "Q")),
                            Eq [Var "D", Var "B"], Var "I"]) = ["A","D","X","Q","B","I"];
val test2 = getVars (Imp (Imp (True, Var 1), Imp (Var 0, Not (Var 4)))) = [1,0,4];
val test3 = getVars exp1 = [1,2,3,4];
val test4 = getVars False = [];
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- eval ----------\n";
val _ : ''a list -> ''a expression -> bool = eval;
val test1 = eval [1, 3] exp1  = true;
val test1 = eval [1, 3] exp2  = true;
val test1 = eval [1, 3] exp3  = true;
val test1 = eval [1, 3] exp4  = true;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- rmEmpty ----------\n";
val _ : 'a expression -> 'a expression = rmEmpty;
val test1 = rmEmpty exp6 = And [Var 1,Not False,False,Eq [Var 1,True],Eq [Var 3,Var 1,Not (Var 4)]];
val test2 = rmEmpty exp7 = Eq [Var 1,True,Not (Var 3),Var 1,Imp (And [True,Not (Var 1),True],False)];
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- beautify ----------\n";
val _ : 'a expression -> 'a expr = beautify;
val test1 = beautify exp1 = V 1 /\ !! (V 2) /\ (V 3 \/ F \/ !! (V 4));
val test2 = beautify exp5 = (V 1 <=> T) /\ (T <=> !! (V 3)) /\ (!! (V 3) <=> V 1 \/ F \/ V 5) /\ (V 1 \/ F \/ V 5 <=> T /\ !! (V 1) ==> F);
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- pushNegations ----------\n";
val _ : 'a expression -> 'a expression = pushNegations;
val test1 = pushNegations (Not (exp1)) = Or [Not (Var 1),Var 2,And [Not (Var 3),Not False,Var 4]];
val test2 = pushNegations (Not (exp5)) = And [Or [Not (Var 1),Not True,Var 3,And [Not (Var 1),Not False,Not (Var 5)], And [And [True,Not (Var 1)],Not False]], Or [Var 1,True,Not (Var 3),Or [Var 1,False,Var 5], Imp (And [True,Not (Var 1)],False)]];
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- rmConstants ----------\n";
val _ : ''a expression -> ''a expression = rmConstants;
val test1 = rmConstants exp3 = And [Var 1,Not (Var 2),And [Var 3,Not (Var 4)]];
val test2 = rmConstants exp5 = And [Var 1,Not (Var 3),Or [Var 1,Var 5],Var 1];
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- rmVars ----------\n";
val _ : ''a expression -> ''a expression = rmVars;
val test1 = rmVars exp8 = And [Var 1,Eq [Var 1,True]];
val test2 = rmVars exp9 = Eq [Var 1,True,Or [Var 1,False],Imp (Var 2,False)];
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- simplify ----------\n";
val _ : ''a expression -> ''a expression = simplify;
val test1 = simplify exp5 = And [Var 1,Not (Var 3),Or [Var 1,Var 5]];
val test2 = simplify exp9 = And [Var 1,Not (Var 2)];
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- prTestEq ----------\n";
val _ : int -> ''a expression -> ''a expression -> bool = prTestEq;
val test1 = List.tabulate (20, (fn i => (i, prTestEq i exp1 exp3))) =
    [(0,true),(1,true),(2,true),(3,true),(4,true),(5,true),(6,true),(7,true),
    (8,true),(9,true),(10,true),(11,true),(12,true),(13,true),(14,true),
    (15,true),(16,true),(17,true),(18,true),(19,true)];
val test2 = List.tabulate (20, (fn i => (i, prTestEq i exp1 exp4))) =
    [(0,true),(1,true),(2,true),(3,true),(4,true),(5,true),(6,true),(7,false),
   (8,true),(9,true),(10,true),(11,true),(12,true),(13,true),(14,true),
   (15,true),(16,true),(17,true),(18,true),(19,true)];
val _ = (all_tests := !all_tests @ [test1, test2]);


val _ = print "---------- toWolframLang ----------\n";
val _ : ('a -> string) -> 'a expression -> string = toWolframLang;
val test1 = toWolframLang Int.toString exp1 = "And[Var[\"1\"], Not[Var[\"2\"]], Or[Var[\"3\"], False, Not[Var[\"4\"]]]]";
val test2 = toWolframLang Int.toString exp8 = "And[Var[\"1\"], Var[\"1\"], Equivalent[Var[\"1\"], Var[\"1\"], Implies[Var[\"2\"], Var[\"2\"]]]]";
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- satSolver ----------\n";
val _ : ''a expression -> ''a list option = satSolver;
val test1 = let val sol = satSolver exp1 in sol =  SOME [1,3] orelse sol = SOME [3,1] end;
val test2 = let val sol = satSolver exp10 in sol = SOME [1] orelse sol = SOME [3] end;
val test3 = satSolver exp11 = NONE;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- bruteforce ----------\n";
val _ : ''a expression -> ''a list option = bruteforce;
val test1 = let val sol = bruteforce exp1 in sol = SOME [1,3] orelse sol = SOME [1,3,4] end;
val test2 = let val sol = bruteforce exp2 in sol = SOME [1,2,3,4] orelse sol = SOME [1] orelse sol = SOME [3] orelse sol = SOME [1, 3] orelse sol = SOME [1, 2] orelse sol = SOME [1, 4] orelse sol = SOME [2, 3] orelse sol = SOME [3, 4] orelse sol = SOME [1,2,3] orelse sol = SOME [1,3,4] orelse sol = SOME [1, 2,4] orelse sol = SOME [2, 3, 4] end;
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- isCNF ----------\n";
val _ : 'a expression -> bool = isCNF
val test1 = isCNF exp1 = true;
val test2 = isCNF exp2 = false;
val test3 = isCNF exp3 = false;
val test4 = isCNF exp4 = false;
val test5 = isCNF exp5 = false;
val test6 = isCNF exp6 = false;
val test7 = isCNF exp7 = false;
val test8 = isCNF exp8 = false;
val test9 = isCNF exp9 = false;
val test10 = isCNF exp10 = true;
val test11 = isCNF exp11 = true;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11]);



val nr_passes_tests = foldl (fn (true, acc) => acc + 1 | (false, acc) => acc) 0 (!all_tests);
val nr_all_tests = length (!all_tests);

val _ = if nr_passes_tests = nr_all_tests
        then OS.Process.exit OS.Process.success
        else OS.Process.exit OS.Process.failure;
