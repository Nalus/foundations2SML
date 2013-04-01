#!/u1/staff/jbw/bin/smlnj-script

(* default input and output files *)
val default_input = "input.json";
val default_output = "output.txt";

(* the program is run as a script to make use of smlnj ability to parse json *)
datatype operator = OP_SET | OP_TUPLE | OP_DOMAIN | OP_RANGE | OP_INVERSE | OP_EQUAL | OP_MEMBER | OP_IS_FUNCTION | OP_IS_INJECTIVE | OP_APPLY_FUNCTION | OP_NTH | OP_UNION | OP_INTERSECTION | OP_SET_DIFFERENCE | OP_DIAGONALIZE;

(* added EXP_SET and EXP_TUPLE wrappers to unify types used *)
datatype expression = EXP_INT of IntInf.int
                    | EXP_VAR of string
                    | EXP_OP of operator * expression list
                    | EXP_SET of expression list
                    | EXP_TUPLE of expression list
                    | EXP_UNDEF;

val undef = "undefined!"

(* start of json parser functions taken from Joe Wells' input-converter script *)
fun operatorToName OP_SET            = "set"
  | operatorToName OP_TUPLE          = "tuple"
  | operatorToName OP_DOMAIN         = "domain"
  | operatorToName OP_RANGE          = "range"
  | operatorToName OP_INVERSE        = "inverse"
  | operatorToName OP_EQUAL          = "equal"
  | operatorToName OP_MEMBER         = "member"
  | operatorToName OP_IS_FUNCTION    = "is-function"
  | operatorToName OP_IS_INJECTIVE   = "is-injective"
  | operatorToName OP_APPLY_FUNCTION = "apply-function"
  | operatorToName OP_NTH            = "nth"
  | operatorToName OP_UNION          = "union"
  | operatorToName OP_INTERSECTION   = "intersection"
  | operatorToName OP_DIAGONALIZE    = "diagonalize"
  | operatorToName OP_SET_DIFFERENCE = "set-difference";

fun nameToOperator "set"               = OP_SET
  | nameToOperator "tuple"             = OP_TUPLE
  | nameToOperator "domain"            = OP_DOMAIN
  | nameToOperator "range"             = OP_RANGE
  | nameToOperator "inverse"           = OP_INVERSE
  | nameToOperator "equal"             = OP_EQUAL
  | nameToOperator "member"            = OP_MEMBER
  | nameToOperator "is-function"       = OP_IS_FUNCTION
  | nameToOperator "is-injective"      = OP_IS_INJECTIVE
  | nameToOperator "apply-function"    = OP_APPLY_FUNCTION
  | nameToOperator "nth"               = OP_NTH
  | nameToOperator "union"             = OP_UNION
  | nameToOperator "intersection"      = OP_INTERSECTION
  | nameToOperator "diagonalize"       = OP_DIAGONALIZE
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
fun getval(a,[]) = EXP_VAR undef
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

(* start of parsing of input file *)
val listok = jsonToStatementList (JSONParser.parseFile default_input);
(* end of initialisation of default input file and then parsing it *)

(* initialisation of output stream *)
val out = TextIO.openOut default_output;

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
      | printS (EXP_SET ((EXP_VAR undef)::t)) = printf undef
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
    | printVal (EXP_VAR undef) = printf (undef^";\n")
    | printVal (EXP_SET s) = (printf "{";  printS (EXP_SET s); printf "};\n")
    | printVal (EXP_TUPLE tu) = (printf "("; printS (EXP_TUPLE tu); printf ");\n")
    | printVal _ = raise (Fail "printVal exception")
end;
(* end of PRINTING FUNCTION FROM PART-1 *)

(* start of new printing functions: table to output stream and syntax error *)
fun toStream [] = (TextIO.flushOut out)
  | toStream ((s:string, e:expression)::tail) = (printf (s^"="); printVal (e); toStream tail);

fun printBadInput  () = printf "BAD INPUT\n";
(* end of function for printing table to output stream *)

(* start of evaluation functions *)
local
  (* return values for variables and integers *)
  fun numValue (EXP_INT i,vars) = EXP_INT i
    | numValue (EXP_VAR vv,vars) = getval (vv,vars)
    | numValue _ = (printBadInput();raise (Fail "numValue exception"));

  (* determine whether element exists in set *)
  fun member (a, EXP_SET set) = if (List.exists (fn x => x=a) set) then EXP_INT 1 else EXP_INT 0
    | member _ = EXP_VAR undef;

  (* determine whether element is a function *)
  fun isFunc (EXP_SET []) = true
    | isFunc (EXP_SET ((EXP_TUPLE [a,b])::t)) = if (List.exists (fn EXP_TUPLE [a,x] => x=b | _ => raise (Fail "this is impossible #0\n")) t) then false else (isFunc (EXP_SET t))
    | isFunc _ = false;

  fun apply ((EXP_TUPLE [x,y])::t,b) = if x=b then y else apply (t,b)
    | apply _ = EXP_VAR undef;

  (* takes in a function, [(),(),...], and an element *)
  fun applyFunc (EXP_SET a,b) = if (isFunc (EXP_SET a)) then apply (a,b) else EXP_VAR undef
    | applyFunc _ = EXP_VAR undef;

  (* returns left elements of all pairs of a function *)
  fun domainFunc (EXP_SET [],domSet) = EXP_SET (rev domSet)
    | domainFunc (EXP_SET (EXP_TUPLE [a,b]::t),domSet) = domainFunc (EXP_SET t,a::domSet)
    | domainFunc _ = EXP_VAR undef;

  (* returns right elements of all pairs of a function *)
  fun rangeFunc (EXP_SET [],rangeSet) = EXP_SET (rev rangeSet)
    | rangeFunc (EXP_SET (EXP_TUPLE [a,b]::t),rangeSet) = rangeFunc (EXP_SET t,a::rangeSet)
    | rangeFunc _ = EXP_VAR undef;

  (* set operation: intersection *)
  fun interFunc (EXP_SET [],EXP_SET b,interSet) = EXP_SET (rev interSet)
    | interFunc (EXP_SET a,EXP_SET [],interSet) = EXP_SET (rev interSet)
    | interFunc (EXP_SET (h1::t1),EXP_SET b,interSet) = if (List.exists (fn x=>x=h1) b) then interFunc(EXP_SET t1,EXP_SET b,h1::interSet) else interFunc(EXP_SET t1,EXP_SET b,interSet)
    | interFunc _ = EXP_VAR undef;

  (* set operation: union *)
  fun garbage [] = []
    | garbage (h::t) = if (List.exists (fn x=>x=h) t) then h::(garbage t) else (garbage t);

  fun unionFunc (EXP_SET [],EXP_SET b) = EXP_SET (b)
    | unionFunc (EXP_SET a,EXP_SET []) = EXP_SET (a)
    | unionFunc (EXP_SET a,EXP_SET b) = EXP_SET (garbage a@b)
    | unionFunc _ = EXP_VAR undef;

  (* set operation: difference, modified interFunc *)
  fun diffFunc (EXP_SET [],EXP_SET b,diffSet) = EXP_SET (rev diffSet)
    | diffFunc (EXP_SET a,EXP_SET [],diffSet) = EXP_SET (rev diffSet)
    | diffFunc (EXP_SET (h1::t1),EXP_SET b,diffSet) = if (List.exists (fn x=>x=h1) b) then diffFunc(EXP_SET t1,EXP_SET b,diffSet) else diffFunc(EXP_SET t1,EXP_SET b,h1::diffSet)
    | diffFunc _ = EXP_VAR undef;
(*    | diffFunc _ = (printBadInput();raise (Fail "diffFunc exception"));*)

  (* inverse function, reverse each pair of the set *)
  fun inverseFunc (EXP_SET [],inverse) = EXP_SET (rev inverse)
    | inverseFunc (EXP_SET ((EXP_TUPLE a)::t),inverse) = inverseFunc (EXP_SET t, EXP_TUPLE (rev a)::inverse)
    | inverseFunc _ = EXP_VAR undef;
(*    | inverseFunc _ = (printBadInput();raise (Fail "inverseFunc exception"));*)

  (* boolean function that determines whether a function is injective, 1:1, only one key to each value *)
  fun isInjFunc (EXP_SET []) = false
    | isInjFunc (EXP_SET ((EXP_TUPLE [a,b])::t)) = if (List.exists (fn EXP_TUPLE [x,b] => x=a | _ => raise (Fail "this is impossible #1\n")) t) then false else (isFunc (EXP_SET t))
    | isInjFunc _ = false;

  fun diagonalize (EXP_SET [],v2,v3) = []
    | diagonalize (EXP_SET ((EXP_TUPLE [a,b])::t),v2,v3) = if (isFunc b) then ((EXP_TUPLE [a,(if ((applyFunc (v2, (applyFunc (b, a)))) = EXP_VAR undef) then v3 else (applyFunc (v2, (applyFunc (b, a)))))])::(diagonalize (EXP_SET t,v2,v3))) else [EXP_INT 0]
    | diagonalize _ = (printBadInput();raise (Fail "diagonalize exception "))

  (* start of mutually recursive declaration, very strong coupling *)
  (* finds list of values from a list, ie converts variables into values, also garbage collects *)
  (* because sets and tuples are implemented as lists, tuples are also garbage collected and are not allowed duplicates *)
  fun setValue ([],vars) = []
    | setValue (EXP_INT h::t,vars) = if (List.exists (fn x => x=(EXP_INT h)) t) then (setValue (t,vars)) else (numValue (EXP_INT h,vars))::(setValue (t,vars))
    | setValue (EXP_VAR h::t,vars) = if (List.exists (fn x => x=(EXP_VAR h)) t) then (setValue (t,vars)) else (numValue (EXP_VAR h,vars))::(setValue (t,vars))
    | setValue (EXP_OP h::t,vars) = if (List.exists (fn x => x=(EXP_OP h)) t) then (setValue (t,vars)) else (opValue (h,vars))::(setValue (t,vars))
    | setValue _ = (printBadInput();raise (Fail "setValue exception "))

  (* tuple evaluation *)
  and tupleValue ([],vars) = []
    | tupleValue (EXP_INT h::t,vars) = (numValue (EXP_INT h,vars))::(tupleValue (t,vars))
    | tupleValue (EXP_VAR h::t,vars) = (numValue (EXP_VAR h,vars))::(tupleValue (t,vars))
    | tupleValue (EXP_OP h::t,vars) = (opValue (h,vars))::(tupleValue (t,vars))
    | tupleValue _ = (printBadInput();raise (Fail "tupleValue exception "))

  (* finds value of an EXP_OP tuple, OP_EQUAL is taken as equality check here *)
  and opValue ((OP_SET, set),vars) = EXP_SET (setValue (set,vars))
    | opValue ((OP_TUPLE, [a]),vars) = expValue (a,vars)
    | opValue ((OP_TUPLE, tuple),vars) = EXP_TUPLE (tupleValue (tuple,vars))
    | opValue ((OP_EQUAL, [a,b]),vars) = if (expValue (a,vars))=(expValue (b,vars)) then EXP_INT 1 else EXP_INT 0
    | opValue ((OP_MEMBER, [a,b]),vars) = member ((expValue (a,vars)),(expValue (b,vars)))
    | opValue ((OP_IS_FUNCTION, [a]),vars) = if (isFunc (expValue (a,vars))) then EXP_INT 1 else EXP_INT 0
    | opValue ((OP_APPLY_FUNCTION, [a, b]),vars) = applyFunc (expValue(a,vars), expValue(b,vars))
    | opValue ((OP_DOMAIN, [a]),vars) = if (isFunc (expValue (a,vars))) then (domainFunc (expValue (a,vars),[])) else EXP_VAR undef
    | opValue ((OP_RANGE, [a]),vars) = if (isFunc (expValue (a,vars))) then (rangeFunc (expValue (a,vars),[])) else EXP_VAR undef
    | opValue ((OP_INTERSECTION, [a,b]),vars) = interFunc ((expValue (a,vars)),(expValue (b,vars)),[])
    | opValue ((OP_UNION, [a,b]),vars) = unionFunc ((expValue (a,vars)),(expValue (b,vars)))
    | opValue ((OP_DIFFERENCE, [a,b]),vars) = diffFunc ((expValue (a,vars)),(expValue (b,vars)),[])
    | opValue ((OP_INVERSE, [a]),vars) = if (isFunc (expValue (a,vars))) then inverseFunc (expValue (a,vars),[]) else EXP_VAR undef
    | opValue ((OP_IS_INJECTIVE, [a]),vars) = if (isFunc (expValue (a,vars))) then (if (isInjFunc (expValue (a,vars))) then EXP_INT 1 else EXP_INT 0) else EXP_VAR undef
    | opValue ((OP_DIAGONALIZE, [v1,v2,v3]),vars) = if ((isFunc (expValue (v1,vars)))(* andalso (isFunc (expValue (v2,vars)))*)) then (EXP_SET (diagonalize (expValue (v1,vars),expValue (v2,vars),expValue (v3,vars)))) else EXP_VAR undef
    | opValue _ = (printBadInput();raise (Fail "opValue exception"))

  (* calculates value of expression arguments; singleton values are returned, operator expressions are passed to opValue *)
  and expValue (EXP_INT i,vars) = numValue (EXP_INT i,vars)
    | expValue (EXP_VAR vv,vars) = numValue (EXP_VAR vv,vars)
    | expValue (EXP_OP oper,vars) = opValue (oper,vars)
    | expValue _ = (printBadInput();raise (Fail "expValue exception"));
  (* end of mutually recursive declaration *)

  (* goes through a list of expressions, performs assignation by storing variable name and its value in a table *)
  (* return a table with key-value pairs, prints each expression after its evaluation *)
  fun evaluator ([],SOME vars) = SOME vars
    | evaluator ((EXP_OP (OP_EQUAL, [EXP_VAR name, arg]))::exp_tail,SOME vars)=
        let val ex = expValue (arg,vars) (* cuts the efficiency in half *)
        in (toStream [(name,ex)];evaluator (exp_tail,insert(name,ex,vars)))
        end
    | evaluator _ = (printBadInput();raise (Fail "evaluator exception"));
in
  val hashMap = evaluator (listok,(SOME ([]:table)));
end;
(* end of evaluation functions *)


(* print the result to the default outfile *)
(*toStream (Option.valOf(hashMap));*)
