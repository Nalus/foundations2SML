datatype mytree = NODE of mydt * mytree * mytree | EMPTY
datatype mydt = INT of int | SET of int list | TUPLE of mydt list

fun printList [] = () 
  | printList li = (print (Int.toString (hd li)); print ",";printList (tl li)) ;

fun printInt a = print (Int.toString a);

fun operator (INT a) = printInt a
  | operator (SET b) = printList b;


(*  input values  *)
val x0 = INT 8;
val x1 = SET [1,2,3,4,5,6,7,x0];
val x2 = TUPLE [x1, TUPLE [INT 1, x1]];

(* process input *)
print "x0  = ";operator x0;print ";\n";
print "x1 = {";printList x1;print "};\n";
