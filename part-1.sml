datatype listok = SET of int list | TUPLE of listok list | INT of int
datatype mytree = NODE of {data:listok, left: mytree, right: mytree} | EMPTY
exception unkownInput


(* comparator for listok *)
fun compare (SET []) (SET []) = EQUAL
  (*| compare (SET (h1::t1)) (SET (h2::t2)) = if h1 = h2 then compare (SET t1) (SET t2) else false*)
  | compare (SET l1) (SET l2) = (case compare (l1.length) (l2.length) of
       LESS    => LESS
    |  GREATER => GREATER
    |  EQUAL   => if sameOrderS(l1,l2) then EQUAL else (if (maxValS l1) > (maxValS l2) then GREATER else LESS))
  | compare (TUPLE []) (TUPLE []) = EQUAL
  (*| compare (TUPLE (h1::t1)) (TUPLE (h2::t2)) = if h1 = h2 then compare (SET t1) (SET t2) else false*)
  | compare (TUPLE t1) (TUPLE t2) = (case compare (t1.length) (t2.length) of
       LESS    => LESS
    |  GREATER => GREATER
    |  EQUAL   => if sameOrderT(t1,t2) then EQUAL else (if (maxValT t1) > (maxValT t2) then GREATER else LESS))
  | compare (INT num1) (INT num2) = Int.compare(num1, num2)
  (* compare for listok cnstructor *)
  | compare (INT _) (SET _) = LESS
  | compare (INT _) (TUPLE _) = LESS
  | compare (SET _) (INT _) = GREATER
  | compare (SET _) (TUPLE _) = LESS
  | compare (TUPLE _) (INT _) = GREATER
  | compare (TUPLE _) (SET _) = GREATER
  | compare _ _ = raise unkownInput
(* end of comparator for listok *)

(* Tree data structure methods *)

(* following functions have been taken from http://en.literateprograms.org/Binary_search_tree_(Standard_ML) *)
fun search(tree:mytree, compare, data:listok) = 
  let fun s(EMPTY) = NONE
    | s(NODE{data=nodedata,left=left,right=right}) = 
      (case compare(data, nodedata) of
        LESS    => s(left)
        | GREATER => s(right)
        | EQUAL   => SOME nodedata)
  in
    s(tree)
end;

fun insert(tree:mytree, compare, data : listok) = 
  let fun i(EMPTY) = NODE{data=data, left=EMPTY, right=EMPTY}
    | i(NODE{data=nodedata,left=left,right=right}) = 
      (case compare(data, nodedata) of
          LESS    => NODE{data=nodedata, left=i(left), right=right}
        | GREATER => NODE{data=nodedata, left=left, right=i(right)}
        | EQUAL   => NODE{data=nodedata, left=left, right=right})
  in
    i(tree)
end;

fun delete(tree : mytree, compare, data : listok) = 
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

