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
  | nameToOperator "set-difference"    = OP_SET_DIFFERENCE;
(*  | nameToOperator n = raise (Fail (q'illegal operator name: [^n]'));*)

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

(*val default_output = "output.txt";*)
(*val out = TextIO.openOut default_output;*)
(*fun toFile s = (TextIO.output(out,s); TextIO.flushOut out)*)
type (''a,'b) table = (''a * 'b) list
exception Table

fun getval(a,[]) = raise Table
  | getval(a,(a1,b1)::t) = if a=a1 then b1 else getval(a,t);
fun insert(a,b,[]) = SOME [(a,b)]
  | insert(a,b,(a1,b1)::t) = if a=a1 then NONE
                            else (case insert (a,b,t) of
                                 NONE => NONE
                               | SOME t1 => SOME((a1,b1)::t1));
val vars = [];


val default_input = "test.json";
val json = JSONParser.parseFile default_input;

val listok = jsonToStatementList json;

fun valueOf (EXP_INT i) = i
  | valueOf (EXP_VAR var) = getval(var,vars)
  | valueOf (EXP_OP (OP_EQUAL, [a,b]))=(if (valueOf a)=(valueOf b) then 0 else 1);


fun valueOfSet (EXP_SET []) = []
  | valueOfSet (EXP_SET (h::t)) = ((valueOf h)::(valueOfSet (EXP_SET t)));

fun evaluator [] = SOME vars
  | evaluator ((EXP_OP (OP_EQUAL, [(EXP_VAR var), (EXP_INT i)]))::exp_tail)=(insert(var,(valueOf (EXP_INT i)),vars);evaluator exp_tail)
  | evaluator ((EXP_OP (OP_EQUAL, [(EXP_VAR var), (EXP_VAR vv)]))::exp_tail)=(insert(var,(valueOf (EXP_VAR vv)),vars);evaluator exp_tail)
  | evaluator ((EXP_OP (OP_EQUAL, [(EXP_VAR var), (EXP_OP (OP_SET, l))]))::exp_tail)=(insert(var,(valueOfSet (EXP_SET l)),vars);evaluator exp_tail);

evaluator listok;
