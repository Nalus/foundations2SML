fun printList [] = () 
  | printList (h::t) = (print (Int.toString h); print ",";printList t) ;

fun printInt a = print (Int.toString a);

(*fun operator (a:int) = printInt a
  | operator (b:int list) = printList b;*)


(*  input values  *)
val x0 = 8;
val x1 = [1,2,3,4,5,6,7,x0];
(*val x2 = (x1, (1,x1));
operator x1;*)

(* process input *)
print "x0  = ";printInt x0;print ";\n";
print "x1 = {";printList x1;print "};\n";
