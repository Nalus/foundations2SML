datatype mydt = INT of int | SET of int list | TUPLE of mydt list
datatype mytree = NODE of {data:mydt, left: mytree, right: mytree} | EMPTY

(*fun printList [] = () 
  | printList li = (print (Int.toString (hd li)); print ",";printList (tl li)) ;


fun printInt a = print (Int.toString a);

fun operator (INT a) = printInt a
  | operator (SET b) = printList b;*)


(* Tree data structure methods *)

(* following functions have been taken from http://en.literateprograms.org/Binary_search_tree_(Standard_ML) *)
fun search(tree:mytree, compare, data:mydt) = 
  let fun s(EMPTY) = NONE
    | s(NODE{data=nodedata,left=left,right=right}) = 
      (case compare(data, nodedata) of
        LESS    => s(left)
        | GREATER => s(right)
        | EQUAL   => SOME nodedata)
  in
    s(tree)
end;

fun insert(tree:mytree, compare, data : mydt) = 
  let fun i(EMPTY) = NODE{data=data, left=EMPTY, right=EMPTY}
    | i(NODE{data=nodedata,left=left,right=right}) = 
      (case compare(data, nodedata) of
        LESS    => NODE{data=nodedata, left=i(left), right=right}
        | GREATER => NODE{data=nodedata, left=left, right=i(right)}
        | EQUAL   => NODE{data=nodedata, left=left, right=right})
  in
    i(tree)
end;

fun delete(tree : mytree, compare, data : mydt) = 
  let
    fun valueMax(NODE{data=nodedata,right=EMPTY,...}) = nodedata
    | valueMax(NODE{right=right,...}) = valueMax(right)
    fun deleteMax(node as NODE{data=nodedata,left=left,right=LEAF}) =
      deleteNode(node)
    | deleteMax(NODE{data=nodedata,left=left,right=right}) =
      NODE{data=nodedata,left=left,right=deleteMax(right)}
    and deleteNode(NODE{data=nodedata,left=EMPTY,right=EMPTY}) = EMPTY
    | deleteNode(NODE{data=nodedata,left=EMPTY,right=right}) = right
    | deleteNode(NODE{data=nodedata,left=left,right=EMPTY})  = left
    | deleteNode(NODE{data=nodedata,left=left,right=right}) =
      NODE{data=valueMax(left), left=deleteMax(left), right=right}
    fun d(EMPTY) = EMPTY (* data was not found *)
    | d(node as NODE{data=nodedata,left=left,right=right}) = 
      (case compare(data, nodedata) of
        LESS    => NODE{data=nodedata,left=d(left),right=right}
      | GREATER => NODE{data=nodedata,left=left,right=d(right)}
      | EQUAL   => deleteNode(node))
  in
    d(tree)
end;

(* functions above have been taken from http://en.literateprograms.org/Binary_search_tree_(Standard_ML) *)

(*  input values  *)
val x0 = INT 8;
val x1 = TUPLE [INT 1,INT 2,INT 3,INT 4,INT 5,INT 6,INT 7,x0];
val x2 = TUPLE [x1, TUPLE [INT 1, x1]];

(* process input *)
(*print "x0  = ";operator x0;print ";\n";
print "x1 = {";printList x1;print "};\n";*)
