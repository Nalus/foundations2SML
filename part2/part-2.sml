#!/u1/staff/jbw/bin/smlnj-script

datatype operator = OP_SET | OP_TUPLE | OP_DOMAIN | OP_RANGE | OP_EQUAL | OP_MEMBER | OP_IS_FUNCTION | OP_APPLY_FUNCTION | OP_NTH | OP_UNION | OP_INTERSECTION | OP_SET_DIFFERENCE;

datatype expression = EXP_INT of IntInf.int
                    | EXP_VAR of string
                    | EXP_OP of operator * expression list
                    | EXP_SET of expression list
                    | EXP_TUPLE of expression list
                    | EXP_UNDEF;

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

type table = (string * expression) list
exception Table

fun getval(a,[]) = raise Table
  | getval(a,(a1,b1)::t) = if a=a1 then b1 else getval(a,t);
fun insert(a,b,[]) = SOME [(a,b)]
  | insert(a,b,(a1,b1)::t) = if a=a1 then NONE
                            else (case insert (a,b,t) of
                                 NONE => NONE
                               | SOME t1 => SOME((a1,b1)::t1));

val default_input = "input.json";
val json = JSONParser.parseFile default_input;

val listok = jsonToStatementList json;


val default_output = "output.txt";
val out = TextIO.openOut default_output;

(* printing function hardwired output stream to namespace "out" *)
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
    | printVal (EXP_SET s) = (printf "{";  printS (EXP_SET s); printf "};\n")
    | printVal (EXP_TUPLE tu) = (printf "("; printS (EXP_TUPLE tu); printf ");\n")
    | printVal _ = raise (Fail "printVal exception")
end;
(* end of PRINTING FUNCTION FROM PART-1 *)

fun toFile [] = (TextIO.flushOut out)
  | toFile ((s:string, e:expression)::tail) = (printf (s^"="); printVal (e); toFile tail);

(* MY LET IS ∨∨∨ *)
let
(* MY LET IS ∧∧∧ *)

fun numValue (EXP_INT i,vars) = EXP_INT i
  | numValue (EXP_VAR vv,vars) = getval (vv,vars)
  | numValue _ = raise (Fail "numValue exception");

fun member (EXP_INT i, EXP_SET []) = false
  | member (EXP_INT i, EXP_TUPLE []) = false
  | member (EXP_INT i, EXP_SET (EXP_INT h::t)) = if i=h then true else (member (EXP_INT i,EXP_SET t))
  | member (EXP_INT i, EXP_TUPLE (EXP_INT h::t)) = if i=h then true else (member (EXP_INT i,EXP_TUPLE t))
  | member _ = raise (Fail "member exception");

fun setValue ([],vars) = []
  | setValue (EXP_INT h::t,vars) = (numValue (EXP_INT h,vars))::(setValue (t,vars))
  | setValue (EXP_VAR h::t,vars) = (numValue (EXP_VAR h,vars))::(setValue (t,vars))
  | setValue (EXP_OP  h::t,vars) = (opValue (h,vars))::(setValue (t,vars))
  | setValue _ = raise (Fail "setValue exception ")

and opValue ((OP_SET, set),vars) = EXP_SET (setValue (set,vars))
  | opValue ((OP_TUPLE, tuple),vars) = EXP_TUPLE (setValue (tuple,vars))
  | opValue ((OP_EQUAL, [a,b]),vars) = if (expValue (a,vars))=(expValue (b,vars)) then EXP_INT 1 else EXP_INT 0
  | opValue ((OP_MEMBER, [a,b]),vars) = if (member ((expValue (a,vars)),(expValue (b,vars)))) then EXP_INT 1 else EXP_INT 0
  | opValue _ = raise (Fail "opValue exception")

and expValue (EXP_INT i,vars) = numValue (EXP_INT i,vars)
  | expValue (EXP_VAR vv,vars) = numValue (EXP_VAR vv,vars)
  | expValue (EXP_OP oper,vars) = opValue (oper,vars)
  | expValue _ = raise (Fail "expValue exception");

fun evaluator ([],SOME vars) = SOME vars
  | evaluator ((EXP_OP (OP_EQUAL, [EXP_VAR name, arg]))::exp_tail,SOME vars)=evaluator (exp_tail,insert(name,(expValue (arg,vars)),vars))
  | evaluator _ = raise (Fail "expression exception");

in evaluator (listok,(SOME ([]:table)))
end;

toFile (Option.valOf(it));
