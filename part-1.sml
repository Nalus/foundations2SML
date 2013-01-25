datatype mydt = INT of int | SET of int list | TUPLE of mydt list
datatype mytree = NODE of {data:mydt, left: mytree, right: mytree} | EMPTY

(*fun printList [] = () 
  | printList li = (print (Int.toString (hd li)); print ",";printList (tl li)) ;


fun printInt a = print (Int.toString a);

fun operator (INT a) = printInt a
  | operator (SET b) = printList b;*)

(* Tree data structure methods *)
val tree : mytree = (-1 * EMPTY * EMPTY);
(*fun addNode (mydt * mytree * mytree) = find leaf*)

(* following functions have been taken from http://en.literateprograms.org/Binary_search_tree_(Standard_ML) *)
fun search(tree:mytree, compare, data:mydt) = 
  let fun s(EMPTY) = -1
  | s(NODE{data=nodedata,left=left,right=right}) = 
    (case compare(data, nodedata) of
      LESS    => s(left)
      | GREATER => s(right)
      | EQUAL   => SOME nodedata)
  in
    s(tree)
  end;

fun insert(tree : 'a tree, compare, data : 'a) = 
    let fun i(Leaf) = Node{data=data, left=Leaf, right=Leaf}
          | i(Node{data=nodedata,left=left,right=right}) = 
                (case compare(data, nodedata) of
                      LESS    => Node{data=nodedata, left=i(left), right=right}
                    | GREATER => Node{data=nodedata, left=left, right=i(right)}
                    | EQUAL   => Node{data=nodedata, left=left, right=right})
    in
        i(tree)
    end

(* functions above have been taken from http://en.literateprograms.org/Binary_search_tree_(Standard_ML) *)

(*  input values  *)
val x0 = INT 8;
val x1 = TUPLE [INT 1,INT 2,INT 3,INT 4,INT 5,INT 6,INT 7,x0];
val x2 = TUPLE [x1, TUPLE [INT 1, x1]];

(* process input *)
(*print "x0  = ";operator x0;print ";\n";
print "x1 = {";printList x1;print "};\n";*)
