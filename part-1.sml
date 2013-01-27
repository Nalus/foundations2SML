datatype listok = SET of listok list | TUPLE of listok list | INT of int
datatype penek = NODE of {data:listok, left: penek, right: penek} | EMPTY

(* printing functions *)
local
  local
    fun isLast [] = true
    | isLast _ = false
  in
    fun printS (INT num) = (print (Int.toString num))
    |  printS (SET []) = ()
    |  printS (SET ((INT h)::t)) = (printS (INT h); if(isLast t) then () else print ","; printS (SET t))
    |  printS (SET ((SET h)::t)) = (print "{"; printS (SET h); print "}"; if(isLast t) then () else print ","; printS (SET t))
    |  printS (SET ((TUPLE h)::t)) = (print "("; printS (TUPLE h); print ")"; if(isLast t) then () else print ","; printS (SET t))
    |  printS (TUPLE []) = ()
    |  printS (TUPLE ((INT h)::t)) = (printS (INT h); if(isLast t) then () else print ","; printS (TUPLE t))
    |  printS (TUPLE ((SET h)::t)) = (print "{"; printS (SET h); print "}"; if(isLast t) then () else print ","; printS (TUPLE t))
    |  printS (TUPLE ((TUPLE h)::t)) = (print "("; printS (TUPLE h); print ")"; if(isLast t) then () else print ",";  printS (TUPLE t))
  end;
in
  fun printVal (INT i) = (print (Int.toString i); print ";\n")
  |  printVal (SET s) = (print "{"; printS (SET s); print "};\n")
  |  printVal (TUPLE tu) = (print "("; printS (TUPLE tu); print ");\n")
end;

fun printPenek (NODE{data=data, left=left, right=right}) = ((if (left = EMPTY) then () else printPenek(left));
  printVal data; (if (right = EMPTY) then () else printPenek(right)))

fun printList (SET []) = ()
|  printList (SET (h::t)) = (printVal h; printList (SET t))
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

(* end of functions that have been taken from http://en.literateprograms.org/Binary_search_tree_(Standard_ML) *)

(* constructor for trees *)
local
  fun constructor tree (SET []) = tree
  |  constructor tree (SET (h::t)) = constructor (insert(tree, h)) (SET t)
in
  fun constructTree li = constructor EMPTY (SET li)
end;
(*end of tree constructor *)

(* SET MANIPULATION functions U, ∩, \ *)
fun concatS (SET []) (INT data) = SET [INT data]
|  concatS (INT data) (SET []) = SET [INT data]
|  concatS (SET s) (INT data) = SET (s@[INT data])
|  concatS (INT data) (SET s) = SET (s@[INT data])
|  concatS (SET s1) (SET s2) = SET (s1@s2)
|  concatS (TUPLE []) (INT  data) = TUPLE [INT data]
|  concatS (INT data) (TUPLE []) = TUPLE [INT data]
|  concatS (TUPLE t) (INT data) = TUPLE (t@[INT data])
|  concatS (INT data) (TUPLE t) = TUPLE (t@[INT data])
|  concatS (TUPLE t) (SET s) = SET (s@t) (* keeps semantics of union correct *)
|  concatS (SET s) (TUPLE t) = SET (s@t)
|  concatS (TUPLE t1) (TUPLE t2) = TUPLE (t1@t2)

fun union (SET s1) (SET []) = SET s1
|  union (SET []) (SET s2) = SET s2
|  union (SET s1) (SET (h2::t2)) = union (if (List.exists (fn x => x=h2) s1) then (SET s1) else (concatS h2 (SET s1))) (SET t2)

local
  fun calculus (SET difference) (SET s1) (SET []) = SET difference
  |  calculus (SET difference) (SET []) (SET s2) = SET difference
  |  calculus (SET difference) (SET (h1::t1)) (SET s2) = calculus (if (List.exists (fn x => x=h1) s2) then (SET difference) else (SET (h1::difference))) (SET t1) (SET s2)
in
  fun diff (SET s1) (SET []) = SET s1
  |  diff (SET []) (SET s2) = SET []
  |  diff (SET s1) (SET s2) = calculus (SET []) (SET s1) (SET s2)
end;

local
  fun bissectrice (SET section) (SET s1) (SET []) = SET section
  |  bissectrice (SET section) (SET []) (SET s2) = SET section
  |  bissectrice (SET section) (SET (h1::t1)) (SET s2) = bissectrice (if (List.exists (fn x => x=h1) s2) then (SET (h1::section)) else (SET section)) (SET t1) (SET s2)
in
  fun inter (SET s1) (SET []) = SET s1
  |  inter (SET []) (SET s2) = SET []
  |  inter (SET s1) (SET s2) = bissectrice (SET []) (SET s1) (SET s2)
end;
(* end of set functions *)

(*  input values  *)
val x0 = INT 8;
val x1 = SET [INT 1,INT 2,INT 3,INT 4,INT 5,INT 6,INT 7,x0];
val x2 = SET [x1, TUPLE [INT 1,x1]];
val x3 = TUPLE [x2,x1];
val x4 = union (SET [x3]) x2;
val x5 = diff x4 (SET [x1]);
val x6 = inter x4 (SET [x1]);

(* prints results via tree data structure, has different order of lines *)
(*constructTree [x0,x1,x2,x3,x4,x5,x6];
printPenek it;
print "**********************END OF TREE PRINTING**********************\n";*)

(* prints results via list data  structure, has the same order as input *)
(*SET [x0,x1,x2,x3,x4,x5,x6];
printList it;
print "**********************END OF LIST PRINTING**********************\n";*)

(* prints exact output needed for part 1 *)
print "x"; print (Int.toString 0); print " = "; printVal x0;
print "x"; print (Int.toString 1); print " = "; printVal x1;
print "x"; print (Int.toString 2); print " = "; printVal x2;
print "x"; print (Int.toString 3); print " = "; printVal x3;
print "x"; print (Int.toString 4); print " = "; printVal x4;
print "x"; print (Int.toString 5); print " = "; printVal x5;
print "x"; print (Int.toString 6); print " = "; printVal x6;
