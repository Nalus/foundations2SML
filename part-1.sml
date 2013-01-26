datatype listok = SET of listok list | TUPLE of listok list | INT of int
datatype penek = NODE of {data:listok, left: penek, right: penek} | EMPTY

(* printing functions *)
local
  fun printS (INT num) = (print (Int.toString num); print ",")
  |  printS (SET []) = ()
  |  printS (SET ((INT h)::t)) = (printS (INT h); printS (SET t))
  |  printS (SET ((SET h)::t)) = (print "{"; printS (SET h); print "}"; printS (SET t))
  |  printS (SET ((TUPLE h)::t)) = (print "("; printS (TUPLE h); print ")"; printS (SET t))
  |  printS (TUPLE []) = ()
  |  printS (TUPLE ((INT h)::t)) = (printS (INT h); printS (TUPLE t))
  |  printS (TUPLE ((SET h)::t)) = (print "{"; printS (SET h); print "}"; printS (TUPLE t))
  |  printS (TUPLE ((TUPLE h)::t)) = (print "("; printS (TUPLE h); print ")"; printS (TUPLE t))
in  fun printVal (INT i) = (print (Int.toString i); print ";\n")
    |  printVal (SET s) = (print "{"; printS (SET s); print "};\n")
    |  printVal (TUPLE tu) = (print "("; printS (TUPLE tu); print ");\n")
end;

fun printPenek (NODE{data=data, left=left, right=right}) = ((if (left = EMPTY) then () else printPenek(left));
  printVal data; print "\n"; (if (right = EMPTY) then () else printPenek(right)))
(* end of printing functions *)


(* helper functions for comparator, mostly for maintainability improvement *)
fun sameOrder (SET []) (SET []) = true
  | sameOrder (SET (h1::t1)) (SET (h2::t2)) = if h1 = h2 then sameOrder (SET t1) (SET t2) else false
  | sameOrder (TUPLE []) (TUPLE []) = true
  | sameOrder (TUPLE (h1::t1)) (TUPLE (h2::t2)) = if h1 = h2 then sameOrder (TUPLE t1) (TUPLE t2) else false

local
  fun helper max (SET []) = max
  | helper max (SET((INT h)::t)) = helper (if h > max then h else max) (SET t)
  | helper max (SET((SET s)::t)) = helper (helper max (SET s)) (SET t)
  | helper max (SET((TUPLE tu)::t)) = helper (helper max (TUPLE tu)) (SET t)
  | helper max (TUPLE []) = max
  | helper max (TUPLE((INT h)::t)) = helper (if h > max then h else max) (TUPLE t)
  | helper max (TUPLE((SET s)::t)) = helper (helper max (SET s)) (TUPLE t)
  | helper max (TUPLE((TUPLE tu)::t)) = helper (helper max (TUPLE tu)) (TUPLE t)
in
  fun maxVal li = helper 0 li
end;

(* end of help functions for comparator *)

(* comparator for listok *)
fun compare ((SET []), (SET [])) = EQUAL
  | compare ((TUPLE []), (TUPLE [])) = EQUAL
  (* compare for listok cnstructors *)
  | compare ((INT _), (SET _)) = LESS
  | compare ((INT _), (TUPLE _)) = LESS
  | compare ((SET _), (INT _)) = GREATER
  | compare ((SET _), (TUPLE _)) = LESS
  | compare ((TUPLE _), (INT _)) = GREATER
  | compare ((TUPLE _), (SET _)) = GREATER
  | compare ((SET l1), (SET l2)) = (case Int.compare((List.length l1), (List.length l2)) of
       LESS    => LESS
    |  GREATER => GREATER
    |  EQUAL   => if(sameOrder (SET l1) (SET l2)) then EQUAL else (if((maxVal (SET l1)) > (maxVal (SET l2))) then GREATER else LESS))
  | compare ((TUPLE t1), (TUPLE t2)) = (case Int.compare((List.length t1), (List.length t2)) of
       LESS    => LESS
    |  GREATER => GREATER
    |  EQUAL   => if(sameOrder (TUPLE t1) (TUPLE t2)) then EQUAL else (if((maxVal (TUPLE t1)) > (maxVal (TUPLE t2))) then GREATER else LESS))
  | compare ((INT num1), (INT num2)) = Int.compare(num1, num2)
(* end of comparator for listok *)

(* Tree data structure methods *)

(* following functions have been taken from http://en.literateprograms.org/Binary_search_tree_(Standard_ML) *)
fun search(tree:penek, data:listok) = 
  let
    fun s(EMPTY) = NONE
    | s(NODE{data=nodedata,left=left,right=right}) = 
      (case compare(data, nodedata) of
        LESS    => s(left)
        | GREATER => s(right)
        | EQUAL   => SOME nodedata)
  in
    s(tree)
end;

fun insert(tree:penek, data : listok) = 
  let
    fun i(EMPTY) = NODE{data=data, left=EMPTY, right=EMPTY}
    | i(NODE{data=nodedata,left=left,right=right}) = 
      (case compare(data, nodedata) of
          LESS    => NODE{data=nodedata, left=i(left), right=right}
        | GREATER => NODE{data=nodedata, left=left, right=i(right)}
        | EQUAL   => NODE{data=nodedata, left=left, right=right})
  in
    i(tree)
end;

fun delete(tree : penek, data : listok) = 
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

local
  fun constructor tree (SET []) = tree
  |  constructor tree (SET (h::t)) = constructor (insert(tree, h)) (SET t)
in
  fun constructTree li = constructor EMPTY (SET li)
end;

(*  input values  *)
val x0 = INT 8;
val x1 = SET [INT 1,INT 2,INT 3,INT 4,INT 5,INT 6,INT 7,x0];
val x2 = SET [x0, x1];
val x3 = SET [TUPLE[INT 1, INT 2], TUPLE[INT 3, INT 4]];
val x8 = TUPLE [TUPLE [INT 0, INT 1], TUPLE [INT 3, TUPLE [INT 4, INT 5]]];

(*val root = constructTree (SET [x0,x1,x2,x3,x8]);*)
val root = insert(EMPTY, x8);
printPenek(root);
