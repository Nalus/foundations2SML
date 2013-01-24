fun printList [] = print "\n" 
  | printList (h::t) = (print (Int.toString h); printList t) ;

fun printInt a = Int.toString a;

fun operator (a:int) = print (printInt a)
  | operator (h::t) = print (printList (h::t)) ;


(*  input values  *)
val x0 = 8;
val x1 = [1,2,3,4,5,6,7,x0];
(*val x2 = (x1, (1,x1));
operator x1;*)
