datatype listok = SET of listok list | TUPLE of listok list | INT of int

local
  fun helper max (SET []) = max
  | helper max (SET((INT h)::t)) = helper (if h > max then h else max) (SET t)
  | helper max (SET((SET s)::t)) = helper (helper max (SET s)) (SET t)
  | helper max (TUPLE []) = max
  | helper max (TUPLE((INT h)::t)) = helper (if h > max then h else max) (TUPLE t)
  | helper max (TUPLE((SET s)::t)) = helper (helper max (SET s)) (TUPLE t)
  | helper max (TUPLE((TUPLE tu)::t)) = helper (helper max (TUPLE tu)) (TUPLE t)
in
  fun maxVal li = helper 0 li
end;

val x8 = TUPLE [TUPLE [INT 0, INT 1], TUPLE [INT 3, TUPLE [INT 4, INT 5]]];
maxVal x8;
