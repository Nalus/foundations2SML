#!/u1/staff/jbw/bin/smlnj-script

(* the program is run as a script to make use of smlnj ability to parse json *)
datatype operator = OP_SET | OP_TUPLE | OP_DOMAIN | OP_RANGE | OP_EQUAL | OP_MEMBER | OP_IS_FUNCTION | OP_APPLY_FUNCTION | OP_NTH | OP_UNION | OP_INTERSECTION | OP_SET_DIFFERENCE;

(* added EXP_SET and EXP_TUPLE wrappers to unify types used *)
datatype expression = EXP_INT of IntInf.int
                    | EXP_VAR of string
                    | EXP_OP of operator * expression list
                    | EXP_SET of expression list
                    | EXP_TUPLE of expression list
                    | EXP_UNDEF;

(* start of json parser functions taken from Joe Wells' input-converter script *)
fun operatorToName OP_SET             = "set"
  | operatorToName OP_TUPLE          = "tuple"
  | operatorToName OP_DOMAIN         = "domain"
  | operatorToName OP_RANGE          = "range"
  | operatorToName OP_EQUAL          = "equal"
  | operatorToName OP_MEMBER         = "member"
  | operatorToName OP_IS_FUNCTION    = "is-function"
  | operatorToName OP_APPLY_FUNCTION = "apply-function"
  | operatorToName OP_NTH            = "nth"
  | operatorToName OP_UNION          = "union"
  | operatorToName OP_INTERSECTION   = "intersection"
  | operatorToName OP_SET_DIFFERENCE = "set-difference";

fun nameToOperator "set"               = OP_SET
  | nameToOperator "tuple"             = OP_TUPLE
  | nameToOperator "domain"            = OP_DOMAIN
  | nameToOperator "range"             = OP_RANGE
  | nameToOperator "equal"             = OP_EQUAL
  | nameToOperator "member"            = OP_MEMBER
  | nameToOperator "is-function"       = OP_IS_FUNCTION
  | nameToOperator "apply-function"    = OP_APPLY_FUNCTION
  | nameToOperator "nth"               = OP_NTH
  | nameToOperator "union"             = OP_UNION
  | nameToOperator "intersection"      = OP_INTERSECTION
  | nameToOperator "set-difference"    = OP_SET_DIFFERENCE
  | nameToOperator n = raise (Fail ("illegal operator name: [^n]"));

fun jsonToExpression (JSON.INT i) = EXP_INT i
  | jsonToExpression (JSON.OBJECT [("variable", JSON.STRING v)]) = EXP_VAR v
  | jsonToExpression (JSON.OBJECT ([("operator", JSON.STRING operName),
                                    ("arguments", JSON.ARRAY args)]
                                   | [("arguments", JSON.ARRAY args),
                                      ("operator", JSON.STRING operName)]))
    = EXP_OP (nameToOperator operName, map jsonToExpression args)
  | jsonToExpression _
    = raise (Fail "illegal JSON that does not represent a math expression");


fun jsonToStatementList (JSON.OBJECT [("statement-list", JSON.ARRAY children)])
    = map jsonToExpression children
  | jsonToStatementList _ = raise (Fail "illegal list of statements");
(* end of json parser functions taken from Joe Wells' input-converter script *)

(* start of declaration of my storage table for expressions: type and functions. *)
(* Taken from "Introduction to Programming using SML" book by Hansen and Rischel *)
type table = (string * expression) list
exception Table

(* gets value given key from table*)
fun getval(a,[]) = EXP_VAR "undefined"
  | getval(a,(a1,b1)::t) = if a=a1 then b1 else getval(a,t);

(* updates value given key in table *)
fun update(a,b,[]) = SOME [(a,b)]
  | update(a,b,(a1,b1)::t) = if a=a1 then SOME ((a,b)::t)
                             else SOME ((a1,b1)::(Option.valOf(update(a,b,t))));

(* inserts key-value pair into table *)
fun insert(a,b,[]) = SOME [(a,b)]
  | insert(a,b,(a1,b1)::t) = if a=a1 then (update(a,b,(a1,b1)::t))
                            else (case insert (a,b,t) of
                                 NONE => NONE
                               | SOME t1 => SOME((a1,b1)::t1));
(* end of declaration of my storage table for expressions: type and functions *)

(* start of initialisation of default input file and then parsing it *)
val default_input = "input.json";
val listok = jsonToStatementList (JSONParser.parseFile default_input);
(* end of initialisation of default input file and then parsing it *)

(* start of initialisation of default output file and stream to it *)
val default_output = "output.txt";
val out = TextIO.openOut default_output;
(* end of initialisation of default output file and stream to it *)

(* printing function hardwired output stream to variable "out" *)
fun printf s = TextIO.output (out,s)

(* start of PRINTING FUNCTION FROM PART-1, modified to print into a stream *)
local
  local
    fun isLast [] = true
    | isLast _ = false
  in
    fun printS (EXP_INT num) = printf (IntInf.toString num)
      | printS (EXP_SET []) = ()
      | printS (EXP_SET ((EXP_INT h)::t)) = (printS (EXP_INT h); if(isLast t) then () else printf ","; printS (EXP_SET t))
      | printS (EXP_SET ((EXP_VAR "undefined")::t)) = printf "underfined"
      | printS (EXP_SET ((EXP_SET h)::t)) = (printf "{"; printS (EXP_SET h); printf "}"; if(isLast t) then () else printf ","; printS (EXP_SET t))
      | printS (EXP_SET ((EXP_TUPLE h)::t)) = (printf "("; printS (EXP_TUPLE h); printf ")"; if(isLast t) then () else printf ","; printS (EXP_SET t))
      | printS (EXP_TUPLE []) = ()
      | printS (EXP_TUPLE ((EXP_INT h)::t)) = (printS (EXP_INT h); if(isLast t) then () else printf ","; printS (EXP_TUPLE t))
      | printS (EXP_TUPLE ((EXP_SET h)::t)) = (printf "{"; printS (EXP_SET h); printf "}"; if(isLast t) then () else printf ","; printS (EXP_TUPLE t))
      | printS (EXP_TUPLE ((EXP_TUPLE h)::t)) = (printf "("; printS (EXP_TUPLE h); printf ")"; if(isLast t) then () else printf ",";  printS (EXP_TUPLE t))
      | printS _ = raise (Fail "printS exception")
  end;
in
  fun printVal (EXP_INT i) = (printf (IntInf.toString i); printf ";\n")
    | printVal (EXP_VAR "undefined") = printf "undefined;\n"
    | printVal (EXP_SET s) = (printf "{";  printS (EXP_SET s); printf "};\n")
    | printVal (EXP_TUPLE tu) = (printf "("; printS (EXP_TUPLE tu); printf ");\n")
    | printVal _ = raise (Fail "printVal exception")
end;
(* end of PRINTING FUNCTION FROM PART-1 *)

(* start of function for printing table to output stream *)
fun toStream [] = (TextIO.flushOut out)
  | toStream ((s:string, e:expression)::tail) = (printf (s^"="); printVal (e); toStream tail);
(* end of function for printing table to output stream *)

(* start of evaluation functions *)
local
  (* return values for variables and integers *)
  fun numValue (EXP_INT i,vars) = EXP_INT i
    | numValue (EXP_VAR vv,vars) = getval (vv,vars)
    | numValue _ = raise (Fail "numValue exception");

  (* determine whether element exists in set or tuple *)
  fun member (a, EXP_SET set) = List.exists (fn x => x=a) set
    | member (a, EXP_TUPLE tuple) = List.exists (fn x => x=a) tuple
    | member _ = raise (Fail "member exception");

  (* start of mutually recursive declaration, very strong coupling *)
  (* finds list of values from a list, ie converts variables into values, also garbage collects *)
  (* because sets and tuples are implemented as lists, tuples are also garbage collected and are not allowed duplicates *)
  fun setValue ([],vars) = []
    | setValue (EXP_INT h::t,vars) = if (List.exists (fn x => x=(EXP_INT h)) t) then (setValue (t,vars)) else (numValue (EXP_INT h,vars))::(setValue (t,vars))
    | setValue (EXP_VAR h::t,vars) = if (List.exists (fn x => x=(EXP_VAR h)) t) then (setValue (t,vars)) else (numValue (EXP_VAR h,vars))::(setValue (t,vars))
    | setValue (EXP_OP h::t,vars) = if (List.exists (fn x => x=(EXP_OP h)) t) then (setValue (t,vars)) else (opValue (h,vars))::(setValue (t,vars))
    | setValue _ = raise (Fail "setValue exception ")

  (* finds value of an EXP_OP tuple, OP_EQUAL is taken as equality check here *)
  and opValue ((OP_SET, set),vars) = EXP_SET (setValue (set,vars))
    | opValue ((OP_TUPLE, tuple),vars) = EXP_TUPLE (setValue (tuple,vars))
    | opValue ((OP_EQUAL, [a,b]),vars) = if (expValue (a,vars))=(expValue (b,vars)) then EXP_INT 1 else EXP_INT 0
    | opValue ((OP_MEMBER, [a,b]),vars) = if (member ((expValue (a,vars)),(expValue (b,vars)))) then EXP_INT 1 else EXP_INT 0
    | opValue _ = raise (Fail "opValue exception")

  (* calculates value of expression arguments; singleton values are returned, operator expressions are passed to opValue *)
  and expValue (EXP_INT i,vars) = numValue (EXP_INT i,vars)
    | expValue (EXP_VAR vv,vars) = numValue (EXP_VAR vv,vars)
    | expValue (EXP_OP oper,vars) = opValue (oper,vars)
    | expValue _ = raise (Fail "expValue exception");
  (* end of mutually recursive declaration *)

  (* goes through a list of expressions, performs assignation by storing variable name and its value in a table *)
  (* return a table with key-value pairs *)
  fun evaluator ([],SOME vars) = SOME vars
    | evaluator ((EXP_OP (OP_EQUAL, [EXP_VAR name, arg]))::exp_tail,SOME vars)=evaluator (exp_tail,insert(name,(expValue (arg,vars)),vars))
    | evaluator _ = raise (Fail "evaluator exception");
in
  val hashMap = evaluator (listok,(SOME ([]:table)));
end;
(* end of evaluation functions *)

(* print the result to the default outfile *)
toStream (Option.valOf(hashMap));
