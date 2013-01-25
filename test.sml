datatype listok = SET of listok list | TUPLE of listok list | INT of int

fun helper max (SET []) = max
| helper max (SET((INT h)::t)) = helper (if h > max then h else max) (SET t)
| helper max (SET((SET s)::t)) = helper (helper max (SET s)) (SET t)

fun maxVal li = helper 0 li
