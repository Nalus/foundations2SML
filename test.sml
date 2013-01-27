datatype listok = SET of listok list | TUPLE of listok list | INT of int

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
  fun printVal (INT i) = (print (Int.toString i); print "};\n")
  |  printVal (SET s) = (print "{"; printS (SET s); print "};\n")
  |  printVal (TUPLE tu) = (print "("; printS (TUPLE tu); print ");\n")
end;

fun union (SET s1) (SET []) = SET s1
|  union (SET []) (SET s2) = SET s2
|  union (SET s1) (SET s2) = SET (s1@s2)

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

val x0 = INT 8;
val x1 = SET [INT 1,INT 2,INT 3,INT 4,INT 5,INT 6,INT 7,x0];
val x2 = SET [x1, TUPLE [INT 1,x1]];
val x3 = TUPLE [x2,x1];
val x4 = union (SET [x3]) x2;
val x5 = union (SET [TUPLE [INT 0, SET [INT 1, INT 2]]]) (SET [TUPLE [INT 3, SET [INT 4, INT 5]]]);
val x5 = diff x4 (SET [x1]); 
val x6 = inter x4 (SET [x1]);
printVal x6;
