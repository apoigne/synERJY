open Ly
open Ast

(* Err.msg  (Err.X()) *)
(* Err.warn (Err.X()) *)

type msg_type =
  | SE
  (* start of type declaration for error messages *)
  | IllegalField of string
  | MainIgnored
  | InvalidPre of string
  | SuperType of string * string * ttype list
  | VectorOp of string * string * ttype
  | ConversionErr of string
  | TargetNoTargetDir
  | TargetNotSupported of string
  | AnonymousOnlyIfc of tclass * tclass
  | InvalidLifecycle of string
  | AbstrCreated of string
  | Access of string * tmfid
  | ACTL of string * tclass * tlbl
  | ActualTypePar of string * ttype * ttype
  | NeverActivated of tclass * tlbl
  | BufferLength of string
  | CircularInherit of tclass list
  | ClockFalse of tclass * tlbl
  | ClocksInconsistent of string * tclass * tlbl
  | ClocksInconsistentInMethodCall of tclass * tlbl
  | ConditionTrans of string
  | ConstructorCycle of tclass * (int list)
  | ConstructorError of tclass * string
  | CTL of string * tclass * tlbl
  | CurrentOnBaseClock of tclass * tlbl
  | CausalityCycle of string list
  | DataPrec of tclass * string * string
  | DeclaredTwice of tclass * tmfid
  | DiagonalOperator of tclass * tlbl
  | DynamicObjectGeneration
  | EmitNotOnBaseClock of tclass * tmfid * tlbl
  | Environment of string
  | ExpandInheritErr of tclass list
  | ExprKindNotReactive of tclass * tlbl
  | FileName of string
  | FlowOutside of string
  | FsmState of string * string
  | IdentifierTwice of tmfid * string
  | IllegalSuper of string * tmfid
  | ImplementedTwice of tclass
  | HomeNotSet
  | Index of tclass * tlbl
  | VerilogIndex of tclass * tlbl
  | InheritErr of tmfid * tclass * string
  | Inits of tmfid
  | InputSignalNotOnBaseClock of tclass * tlbl * tmfid
  | InstantLoop of tclass * tlbl
  | InvalidVersion of string
  | LengthATL of string
  | LengthParamL of string * tmfid
  | Lhs0Err of string * tmfid
  | Lhs1Err of string * tmfid
  | Lhs2Err of string * tmfid
  | Lhs3Err of string * tmfid
  | LongJmp
  | LTL of string * tclass * tlbl
  | Main of tclass
  | MissingClass of tclass
  | NYI of string
  | NoClass of string
  | NoCode
  | NoConfClass
  | NoLogic of string * string
  | NoSuchExtension
  | NoSuchImpl
  | NoVerTool of string
  | NotLocalizedRo of textid
  | NotLocalizedSig of textid
  | OnlyConf
  | InconsistentClocks of string * string * string
  | InconsistentClocksInConstructorCall of tclass * tlbl * string * string
  | InconsistentClocksInNodeCall of tclass * tlbl
  | InvalidPrec of tclass * string
  | RecursiveCallOfReactiveMethod of tclass * tmfid
  | RecursiveCallOfNode of tclass * tmfid
  | ScicosIO of string * string
  | ScicosPar of string * string
  | SfunctionClock of string
  | SfunctionInput of string
  | SfunctionOutput of string
  | SigClock of string
  | SigIn of string * tclass * tlbl
  | SigOut of string * tclass * tlbl
  | SigOutArray of string * tclass * tlbl
  | SigVal1 of string * string
  | SigVal2 of string * string
  | SigVal3 of string * string
  | SigVal4 of string * string
  | StateNotDefined of tclass * tlbl * tmfid
  | Subtyping of string * ttype * ttype
  | TagError of string * string
  | TimeGranularity of string * string * tclass * tlbl
  | TimeRaceInClass of tclass * string * string
  | MultEmitInClass of tmfid * string * string
  | MultEmitInClassSame of tmfid * string
  | MultEmitInAppl of string * tmfid * string * tmfid
  | TransPriority of string
  | Transition of string * string
  | TwoClasses of tclass * string
  | TwoFiles of tclass * string * string
  | ClassMthdAndTypPar of tclass
  | SimulinkInputType of tclass * tmfid
  | SimulinkOutputType of tclass * tmfid
  | SimulinkParamType of ttype
  | SMV of string
  | ThrowNumber of string
  | TypeparInherit of tclass * tclass
  | TypeparIsClass of tclass
  | Typing of string
  | Unchecked of tclass
  | Undeclared of string * string
  | UnitTest of string
  | UnknownId of tclass * tmfid * int * string
  | UnsafeConf
  | UnsupportedPlatform of string
  | UnusedCode of tclass * tlbl
  | UnusedStates of (tmfid list)
  | VerilogNYI of tclass * tlbl
  | VerilogNA of string * tclass * tlbl
  | VerilogIO of string
  | WhenOnTrue of tclass * tlbl
  | WhenOnFalse of tclass * tlbl
  | Input of tclass * tlbl * textid * tclass
  | SimInput of tclass * tlbl * textid
  | InputIgnored of tclass * tlbl * textid * tclass
  | Output of tclass * tlbl * textid * tclass
  | SimOutput of tclass * tlbl * textid
  | OutputIgnored of tclass * tlbl * textid * tclass
(* end of type declaration for error messages *)

(* -------------------------------------------------------------------------
type converting functions
------------------------------------------------------------------------- *)
let ctyp2typ t =
  if t = id_cuint7 || t = id_cint8 then id_char else
  if t = id_cuint8 then id_byte else
  if t = id_cuint15 || t = id_cint16 then id_short else
  if t = id_cuint16 then id_uint16 else
  if t = id_cuint31 || t = id_cint32 then id_int else
  if t = id_cuint32 then id_uint32 else
  if t = id_cuint63 || t = id_cint64 then id_long else
  if t = id_cuint64 then id_uint64 else
    t
let rec dim2str =
  function
  | Arbitrary -> ""
  | DimRef r -> dim2str (! !r)
  | DimVar id -> mf2str id
  | DimLen x -> string_of_int x

let rec type2str typ =
  match typ with
  | Array(t, Arbitrary, Arbitrary) -> type2str t^"[,]"
  | Array(t, d1, d2) when d1 = Arbitrary || d2 = Arbitrary -> type2str t^"[]"
  | Array(t, DimLen 1, d2) -> type2str t^"["^dim2str d2^"]"
  | Array(t, d1, d2) -> type2str t^"["^dim2str d1^","^dim2str d2^"]"
  | Typ(cid, atl) -> (c2str cid)^(atl2string atl)
  | Simple(cid) -> c2str (ctyp2typ cid)
  | TypeVar(n) -> c2str n
  | Any -> c2str id_any
  | Null -> c2str id_null

and atl2string =
  function
  | [] -> ""
  | [Null] -> "Null"
  | atl ->
      let rec l2string =
        function
        | [] -> raise (Error "[[InternalError]] atl2string")
        | [t] -> type2str t
        | t:: tl -> (type2str t)^","^(l2string tl)
      in
      "<"^(l2string atl)^">"

and fp2string f =
  (mf2str f.p_name)^" : "^(type2str f.p_type)

and fpl2string =
  function
  | [] -> ""
  | atl ->
      let rec l2string =
        function
        | [] -> raise (Error "[[InternalError]] fpl2string")
        | [t] -> fp2string t
        | t:: tl -> (fp2string t)^", "^(l2string tl)
      in
      " ( "^(l2string atl)^" )"

let tgtsys2str =
  function
  | Simulation -> "Simulation"
  | Host -> "Host"
  | Makefile -> "Makefile"
  | Simulink -> "Simulink"
  | Scicos -> "Scicos"
  | Platform s -> s
  | VerilogSimulation -> "Verilog Simulation"
  | Verilog s -> s
  | Verification -> "Verification"

(* --------------------------------------------------------------------------
SOURCE - FILE POSITION RELATED FUNCTIONS: convert positions to strings
-------------------------------------------------------------------------- *)
let srcp2str =
  function
  | NoSrcPos -> " [[no source position]] "
  | HaltPos(fid, go, fl, fc, tl, tc)
  | EmitPos(fid, go, fl, fc, tl, tc)
  | TextPos(fid, go, fl, fc, tl, tc) ->
      "\n        file    : "^fid^
      (if go = 0 then "" else
          "\n        graphic object number: "^
          (string_of_int go)
      )^"\n        pos     : "^
      (string_of_int fl)^"."^(string_of_int fc)^
      " : "^
      (string_of_int tl)^"."^(string_of_int tc)
  | GraphicPos(fid, go) ->
      "\n        file:                  "^fid^
      "\n        graphic object number: "^
      (string_of_int go)

let cid_lbl2str cid lbl =
  try
      let ast = Hashtbl.find se.classtab cid in
      let ast =
        match ast.classkind with
        | Anonymous cid -> Hashtbl.find se.classtab cid
        | _ -> ast in
      srcp2str (Ly.lbl2srcp lbl ast.lbl2sp)
  with Not_found ->
      raise (Error("cid_lbl2str:invclass"))

let stmt2lbl = function
  | Activate (l, _, _, _) -> l
  | AssertStmt (l, _) -> l
  | TextStM (l, _) -> l
  | GraphicStM (l, _) -> l
  | Await (l, _, _) -> l
  | Break (l, _) -> l
  | Continue (l, _) -> l
  | DoStmt (l, _, _) -> l
  | Emit (l, _, _, _, _) -> l
  | ExprStmt (l, _) -> l
  | ForStmt (l, _) -> l
  | Halt (l) -> l
  | LetStmt (l, _) -> l
  | Next (l) -> l
  | NextState (l, _) -> l
  | Nothing (l) -> l
  | FlowContext (l, _) -> l
  | Par (l, _) -> l
  | Cancel (l, _, _, _, _) -> l
  | RctLoop (l, _) -> l
  | Return (l, _) -> l
  | Schedule (l, _) -> l
  | Sustain (l, _) -> l
  | Switch (l, _) -> l
  | Throw (l, _) -> l
  | While (l, _, sl) -> l

(* -------------------------------------------------------------------------- *)
let sub_typing t1 t2 =
  if is_literal_type t1 then (
    ":\nthe literal has a value which does not fit to the expected type "^
    type2str t2^" (or a subtype),\n"
  ) else if (match t1 with Array _ -> true | _ -> false) then (
    ":\ninferred type is "^type2str t1^
    ", but expected is "^type2str t2^". Note that subtyping is not \
    supported for arrays. This is on purpose. If values are of \
    primitive type, the programmer should be aware of the fact that \
    the format of all data are changed (at computational and memory \
    costs). If the values are of reference type upcast causes a \
    covariance problem invariably leading to a runtime error.\n"
  ) else (
    ":\ninferred type is "^type2str t1^
    ", but expected is "^type2str t2^" (or a subtype),\n"
  )

let most_gen_type tl =
  String.concat ", " (List.map type2str tl)

let rec dotted2str =
  function
  | [Call(n, _), _] -> mf2str n
  | (Call(n, _), _):: l -> (mf2str n)^"."^(dotted2str l)
  | _ -> raise (Error "Internal Error of dotted2str")

let text =
  function
  | SE -> "[[SE]] mail: axel.poigne@iais.fraunhofer.de\n"
  
  | AbstrCreated(cf) -> "\n[[AbstrCreated]] 'new' for an abstract class illegal. "^cf
  | Access(cf, fid) -> "\n[[Access]] Access to method/field '"^(mf2str fid)^"' illegal, "^cf
  | ACTL(txt, cid, pos) -> "\n[[ACTL]] "^txt^(cid_lbl2str cid pos)
  | ActualTypePar(cf, t1, t2) -> "\n[[ActualTypePar]] Actual type parameter "^(sub_typing t1 t2)^cf
  | NeverActivated(cid, fp) -> "\n[[NeverActivated]] Reaction never activated"^(cid_lbl2str cid fp)
  | BufferLength(s) -> "\n[[BufferLength]] The length of the buffer of signal '"^s^"' cannot be determined.\n"
  | CircularInherit(cid) -> "\n[[CircularInherit]] Inherit-cyle detected for the classes\n    "^(String.concat "\n    " (List.map c2str cid))
  | ClockFalse(cid, sp) -> "\n[[ClockFalse]] The term 'false' is used as a clock"^(cid_lbl2str cid sp)
  | ClocksInconsistent(op, cid, pos) -> "\n[[ClocksInconsistent]] Inconsistent clocks "^op^(cid_lbl2str cid pos)
  | ClocksInconsistentInMethodCall(cid, pos) -> "\n[[ClocksInconsistentInMethodCall]] Inconsistent clocks"^(cid_lbl2str cid pos)
  | ConditionTrans(state) -> "\n[[ConditionTrans]] Invalid condition/transition/init connected to state\n    "^state
  | ConstructorCycle(cid, arityl) -> "\n[[ConstructorCycle]] The constructors of arity\n   "^(String.concat "\n    " (List.map string_of_int arityl))^"\n from class "^c2str cid^" call each other in a cycle.\n"
  | ConstructorError(cid, what) -> "\n[[ConstructorError]] Class "^(c2str cid)^" should have "^what^"constructor\n"
  | CurrentOnBaseClock(cid, pos) -> "\n[[CurrentOnBaseClock]] Some 'current' operator upsamples the base clock"^(cid_lbl2str cid pos)
  | CausalityCycle(sl) -> "[[CausalityCycle]] Causality cycle found. Involved are: "^(List.fold_left (fun l x -> l^"\n   "^x) "" sl)
  | DataPrec(cid, f, g) -> "\n[[DataPrec]] The methods, fields and/or (attached) labels\n    "^f^"\nand\n    "^g^"\nin class\n    "^(c2str cid)^"\nmay be involved in inconsistent precedence-declarations.\n"
  | CTL(txt, cid, pos) -> "\n[[CTL]] "^txt^(cid_lbl2str cid pos)
  | DeclaredTwice(cid, name) -> "\n[[DeclaredTwice]] The method or field '"^mf2str name^"' has been declared twice in class '"^c2str cid^"'.\n"
  | DiagonalOperator(cid, pos) -> "\n[[DiagonalOperator]] The result type of a diagonal operator must be a square matrix"^cid_lbl2str cid pos^"'.\n"
  | DynamicObjectGeneration -> "\nobjects are generated dynamically. Check carefully whether this takes place within reactive code. Then system may be flooded by objects at run time.\n"
  | EmitNotOnBaseClock(cid, id, pos) -> "\n[[EmitNotOnBaseClock]] the signal "^(mf2str id)^" emitted at"^(cid_lbl2str cid pos)^"\nis not on base clock\n"
  | Environment(s) -> "\n[[Environment]] "^s
  | ExpandInheritErr(cidl) -> "\n[[ExpandInheritErr]] cyclic dependency of classes when class extension and field expansion is done. Classes of the cycle are: "^(String.concat ", " (List.map c2str cidl))^". Either reactive classes declare fields directly or indirectly of itself (that is an error) or, fields are defined in a way that expansion analysis decides to expand them (e.g. fields are used only once in a constructor or at declaration) and there are cyclic dependies between such fields (that is annoying, mail reinhard.budde@ais.fhg.de :-<)\n"
  | ExprKindNotReactive(cid, pos) -> "\n[[ExprKindNotReactive]] data expression contains parts which are currently not allowed in reactive expressions"^(cid_lbl2str cid pos)
  | FileName(s) -> "\n[[FileName]] file "^s^" cannot be processed\n"
  | FlowOutside(cl) -> "\n[[FlowOutside]] flow types or flow expressions are illegal outside of a flow definition context {| ...; ... |}. "^cl
  | FsmState(cf, s) -> "\n[[FsmState]] invalid automaton/synERJYchart: "^s^". "^cf
  | InconsistentClocksInNodeCall(cid, pos) -> "\n[[InconsistentClocksInNodeCall]] Parameters of a node that have the same clock must have arguments with the same clock. We have found two arguments of a node call that violate this condition. The position of the second argument is"^(cid_lbl2str cid pos)
  | IdentifierTwice(id, pos) -> "\n[[IdentifierTwice]] identifier "^(mf2str id)^" is used twice in "^pos
  | IllegalSuper(cl, fm) -> "\n[[IllegalSuper]] identifier "^(mf2str fm)^" is invalid in super(). "^cl
  | ImplementedTwice(cid) -> "\n[[ImplementedTwice]] in class "^(c2str cid)^" an interface is implemented twice\n"
  | HomeNotSet -> "\n[[HomeNotSet]] The environment variable 'HOME' is not set"
  | Index(cid, pos) -> "\n[[Index]] The index exceeds the size of a vector or matrix"^(cid_lbl2str cid pos)
  | VerilogIndex(cid, pos) -> "\n[[VerilogIndex]] The dimension of the vector or matrix cannot be computed at compile time, but must be known for verilog compilation\n"^(cid_lbl2str cid pos)
  | InheritErr(fm, cid, s) -> "\n[[InheritErr]] invalid inheritance of method/field \""^(mf2str fm)^"\" by class "^(c2str cid)^": "^s
  | Inits(state) -> "\n[[Inits]] initialization inside of state \n\n  "^(mf2str state)^"\n\n is invalid\n"
  | InputSignalNotOnBaseClock(cid, lbl, id) -> "\n[[InputSignalNotOnBaseClock]] Input signal \n    "^(mf2str id)^"\n is not on base clock at"^(cid_lbl2str cid lbl)
  | InstantLoop(cid, fp) -> "\n[[InstantLoop]] instantaneous loop detected"^(cid_lbl2str cid fp)
  | InvalidVersion(fn) -> "\n[[InvalidVersion]] file "^fn^" contains invalid graphic data, for instance because the graphics may have been generated with an old version. In that case use the graphic editor to reedit the graphics.\n"
  | LengthATL(cf) -> "\n[[LengthATL]] wrong number of actual type parameter, "^cf
  | LengthParamL(cf, fid) -> "\n[[LengthParamL]] wrong number of actual parameter, "^cf^ " identifier "^(mf2str fid)
  | Lhs0Err(cf, fid) -> "\n[[Lhs0Err]] in a class method illegal access to identifier "^(mf2str fid)^". "^cf
  | Lhs1Err(cf, f) -> "\n[[Lhs1Err]] identifier "^(mf2str f)^" illegal at left-hand side. "^cf
  | Lhs2Err(cf, f) -> "\n[[Lhs2Err]] inherited field "^(mf2str f)^" illegal at left-hand side. "^cf
  | Lhs3Err(cf, fid) -> "\n[[Lhs3Err]] the identifier "^(mf2str fid)^" is illegal at left hand side of an assignment at "^cf
  | LongJmp -> "\n[[LongJmp]] the static final field \"system_exception_longjmp\" must be a compile time constant AND not be 0 to conform with the ANSI-C standard --- I never expected this error to occur, congrat by R.Budde\n"
  | LTL(txt, cid, pos) -> "\n[[LTL]] "^txt^txt^(cid_lbl2str cid pos)
  | Main(cid) -> "\n[[Main]] \" main\" in class "^(c2str cid)^" is illegally overloaded. Its only use is to define \"public static void main (String[] args)\" to start the application\n"
  | MissingClass(cid) -> "\n[[MissingClass]] class "^(c2str cid)^" is missing, and must be loaded\n"
  | NYI(text) -> "\n[[NYI]] "^text
  | NoClass(cstr) -> "\n[[NoClass]] The name "^cstr^" does not refer to a class\n"
  | NoCode -> "\n[[NoCode]] no code generated\n"
  | NoConfClass -> "\n[[NoConfClass]] There is no reactive class specified, hence no configuration class can be determined."
  | NoLogic(tool, logic) -> "\n[[NoLogic]] Verification tool "^tool^" with logic "^logic^" is not (yet) supported\n"
  | NoSuchExtension -> "\n[[NoSuchExtension]] illegal to extend or implement a configuration or reactive or final class and illegal to extend an interface\n"
  | NoSuchImpl -> "\n[[NoSuchImpl]] illegal to implement something other as an interface\n"
  | NoVerTool(tool) -> "\n[[NoVerTool]] Verification tool "^tool^" is not (yet) supported\n"
  | NotLocalizedRo(bel) -> "\n[[NotLocalizedRo]] constructor call for reactive object "^(dotted2str bel)^" missing or invalid. Note: signals may only be passed to constructors as field names or formal parameter names, not as more complex expressions\n"
  | NotLocalizedSig(bel) -> "\n[[NotLocalizedSig]] signal "^(dotted2str bel)^" not mapped to the signal bus\n"
  | OnlyConf -> "\n[[OnlyConf]] triggering an instant by evaluating instant() is allowed for configuration classes only, which is recognized by defining public static void main (String[] args)\n"
  | InconsistentClocks(oid, ap, fp) -> "\n[[InconsistentClocks]] the field "^ap^" and the constructor parameter "^fp^" of the class "^oid^" have inconsistent clocks\n"
  | InconsistentClocksInConstructorCall(cid, sp, ap, fp) -> "\n[[InconsistentClocksInConstructorCall]] In object "^c2str cid^": the clock of the actual parameter \n    "^ap^"\n is inconsistent with the clock of the parameter\n    "^fp^"\n of the constructor call"^cid_lbl2str cid sp^".\n"
  | InvalidPrec(cid, txt) -> "\n[[InvalidPrec]] class "^(c2str cid)^" has an invalid precedence declaration: "^txt
  | RecursiveCallOfReactiveMethod(cid, id) -> "\n[[RecursiveCallOfReactiveMethod]] The reactive method "^(mf2str id)^" of class "^(c2str cid)^" is called recursively. This is not allowed for reactive methods."
  | RecursiveCallOfNode(cid, id) -> "\n[[RecursiveCallOfNode]] The node "^(mf2str id)^" of class "^(c2str cid)^" is called recursively This is not allowed for nodes."
  | SfunctionClock(s) -> "\n[[SfunctionClock]] The signal \'"^s^"\' is not on base clock. Input and output signals must be on base clock when generating a Sfunction \n"
  | ScicosIO(s, t) -> "\n[[ScicosIO]] The input or output signal "^s^" has value type "^t^". This type is not admissable for input or output signals in Scicos."
  | ScicosPar(s, t) -> "\n[[ScicosPar]] The parameter "^s^" has value type "^t^". This type is not admissable for parameters in Scicos."
  | SfunctionInput(s) -> "\n[[SfunctionInput]] Only valued signals are allowed as inputs for Sfunctions. The signal \'"^s^"\' is pure.\n"
  | SfunctionOutput(s) -> "\n[[SfunctionOutput]] Only valued signals are allowed as outputs for Sfunctions. The signal \'"^s^"\' is pure.\n"
  | SigClock(cf) -> "\n[[SigClock]] Clock expressions must be of bool type. "^cf
  | SigIn(s, cid, sp) -> "\n[[SigIn]] The type of values of the input signal \'"^s^"\' is not yet supported by the simulator. At:\n     "^cid_lbl2str cid sp
  | SigOut(s, cid, sp) -> "\n[[SigOut]] The type of values of the output signal \'"^s^"\' is not yet supported by the simulator. At:\n     "^cid_lbl2str cid sp
  | SigOutArray(s, cid, sp) -> "\n[[SigOutArray]] The signal has a value being an empty  vector or matrix. At:\n     "^cid_lbl2str cid sp
  | SigVal1(s, pos) -> "\n[[SigVal1]] if the type of the valued signal \'"^s^"\' is no builtin type, it must be a final class with fields of builtin types only. At:\n     "^pos
  | SigVal2(s, pos) -> "\n[[SigVal2]] The class used as value type for the  signal "^s^" must have a constructor without arguments to initialize the signal value. At:\n     "^pos
  | SigVal3(s, pos) -> "\n[[SigVal3]] If the value type of the signal \'"^s^"\' is a vector or matrix with specified dimension, its values must be of  type float or double. At:\n     "^pos
  | SigVal4(s, pos) -> "\n[[SigVal4]] Only vectors and matrices are admissable as values of a signal \'"^s^"\'. At:\n     "^pos
  | StateNotDefined(cid, lbl, id) -> "\n[[StateNotDefined]] State \n    "^(mf2str id)^"\n not defined"^(cid_lbl2str cid lbl)
  | Subtyping(cf, t1, t2) -> "\n[[Subtyping]] typechecking expr/stmt"^(sub_typing t1 t2)^cf
  | TagError(c, t) -> "\n[[TagError]] in class "^c^" the label "^t^" could not be processed (is it missing?)\n"
  | TimeGranularity(time, timescale, cid, lbl) -> "\n[[TimeGranularity]] The time constant '"^time^"' is not a multiple of the specified time granularity '"^timescale^"' micro seconds."^(cid_lbl2str cid lbl)
  | TimeRaceInClass(cid, se1, se2) -> "\n[[TimeRaceInClass]] time race in class\n    "^(c2str cid)^"\n of\n        "^se1^" and "^se2
  | MultEmitInClass(sid, lbl1, lbl2) -> "\n[[MultEmitInClass]] Multiple emittance of the signal \n   "^(mf2str sid)^"\n     at:"^lbl1^"\n     and at:"^lbl2
  | MultEmitInClassSame(sid, lbl1) -> "\n[[MultEmitInClassSame]] Multiple emittance of the signal \n   "^(mf2str sid)^"\n     at:"^lbl1^"\nThe emit statement or flow equation is executed twice at an instant.\n"
  | MultEmitInAppl(oid1, sid1, oid2, sid2) -> "\n[[MultEmitInAppl]]\nTime race between signal \n    "^(mf2str sid1)^" in object "^oid1^"\nand signal\n    '"^mf2str sid2^"' in object "^oid2
  | TransPriority(state) -> "\n[[TransPriority]] illegal priorities of transitions from state "^state
  | Transition(frs, tos) -> "\n[[Transition]] invalid transition from/in state "^frs^" to "^tos
  | TwoClasses(cid, file) -> "\n[[TwoClasses]] class '"^c2str cid^"' more than once in file "^file^". Compilation aborted\n"
  | TwoFiles(cid, f1, f2) -> "\n[[TwoFiles]] class '"^c2str cid^"' from file "^f2^" found in "^f1^". Compilation aborted\n"
  | ClassMthdAndTypPar(cid) -> "\n[[ClassMthdAndTypPar]] class "^(c2str cid)^" has type parameters. Such a class may not have class methods\n"
  | SimulinkInputType(cid, sid) -> "\n[[SimulinkInputType]] The input signal '"^mf2str sid^"' in class '"^c2str cid^"' is either a pure signal or its value is of a reference type or array type. Simulink cannot handle signals of this kind."
  | SimulinkOutputType(cid, sid) -> "\n[[SimulinkOutputType]] The output signal '"^mf2str sid^"' in class '"^c2str cid^"' is either a pure signal or its value is of a reference type or array type. Simulink cannot handle signals of this kind."
  | SimulinkParamType(typ) -> "\n[[SimulinkParamType]] Simulink does not support parameters of type "^type2str typ
  | SMV (txt) -> "\n[[SMV]] "^txt
  | ThrowNumber pos -> "\n[[ThrowNumber]]| User defined exception should have a number greater or equal to 1000 at "^pos
  | TypeparInherit(pcid, ccid) -> "\n[[TypeparInherit]] Class/Interface "^c2str ccid^" inherits (extends or implements) from class/interface "^(c2str pcid)^". But their type parameters do not fit"
  | TypeparIsClass(cid) -> "\n[[TypeparIsClass]] It is hard to read to use "^(c2str cid)^" both as formal type parameter and as class/interface name, and thus forbidden\n"
  | Typing(t) -> "\n[[Typing]] typing error, "^t
  | Unchecked(cid) -> "\n[[Unchecked]] class "^ (c2str cid)^" used, but this class is not yet (type-)checked\n"
  | UnitTest txt -> "\n[[UnitTest]] "^txt
  | Undeclared(what, name) -> "\n[[Undeclared]] the "^what^" is undeclared: "^name
  | UnknownId(cid, fm, len, pos) -> "\n[[UnknownId]] "^(if len = (- 1) then "field '" else "method '")^(mf2str fm)^(if len = (- 1) then "'" else "' with arity [number of parameters] '"^(string_of_int len))^"' is not known in class '"^(c2str cid)^"'. Detected at "^pos
  | UnsafeConf -> "\n[[UnsafeConf]] what about the declared configuration class (not yet loaded?)?\n"
  | UnsupportedPlatform(p) -> "\n[[UnsupportedPlatform]] this platform ("^p^") is not supported by the tk-binding of sE\n"
  | UnusedCode(cid, fp) -> "\n[[UnusedCode]] unused code found starting"^(cid_lbl2str cid fp)
  | UnusedStates(statel) -> "\n[[UnusedStates]] states unused: "^String.concat " " (List.map (fun s -> mf2str s) statel)
  | VerilogNYI (cid, fp) -> "\n[[VerilogNYI]] Not yet supported for Verilog code generation"^cid_lbl2str cid fp
  | VerilogNA (txt, cid, fp) -> "\n[[VerilogNA]] "^txt^" "^cid_lbl2str cid fp
  | VerilogIO (txt) -> "\n[[VerilogIO]] "^txt
  | WhenOnTrue(cid, fp) -> "\n[[WhenOnTrue]] down-sampling using clock true is useless at"^(cid_lbl2str cid fp)
  | WhenOnFalse(cid, fp) -> "\n[[WhenOnTrue]] down-sampling using clock false is useless at"^(cid_lbl2str cid fp)
  | InvalidLifecycle(file) -> "\n[[InvalidLifecycle]] state lifecycle from file "^file^" must not define entry, during, or exit actions (in spite of the fact that the editor allows that to do)\n"
  | AnonymousOnlyIfc(ctxt, ifc) -> "\n[[AnonymousOnlyIfc]] a anonymous class defined in class\n    "^c2str ctxt^"\n is based on\n   "^c2str ifc^"\n which must be an interface\n"
  | Input(cid, fp, name, cbcid) -> "\n[[Input]] input signal \n    "^dotted2str name^cid_lbl2str cid fp^"\n uses a callback object of type \n    "^c2str cbcid^".\n This type either does not implement the marker interface Input or does not export the public method bool new_val(), and -if its a valued signal- T get_val()\n"
  | SimInput(cid, fp, name) -> "\n[[SimInput]] input signal \n    "^dotted2str name^cid_lbl2str cid fp^"\nuses a callback object of type SimInput but code is generated for the target system '"^tgtsys2str se.target_sys^"'."
  | InputIgnored(cid, fp, name, cbcid) -> "\ninput signal \n    "^dotted2str name^cid_lbl2str cid fp^"\n got a callback object of type \n    "^c2str cbcid^".\n This object is simply ignored because a simulation system is generated\n"
  | Output(cid, fp, name, cbcid) -> "\noutput signal \n    "^dotted2str name^cid_lbl2str cid fp^"\n uses a callback object of type \n    "^c2str cbcid^".\n This type either does not implement the marker interface Output or does not export the public method void put_val() resp. void put_val(T) if it is a valued signal\n"
  | SimOutput(cid, fp, name) -> "\n[[SimOutput]] input signal \n    "^dotted2str name^cid_lbl2str cid fp^"\n uses a callback object of type SimOutput but code is generated for a target system '"^tgtsys2str se.target_sys^"'."
  | OutputIgnored(cid, fp, name, cbcid) -> "\n[OutputIgnored] output signal \n    "^dotted2str name^cid_lbl2str cid fp^"\n got a callback object of type \n    "^c2str cbcid^".\n This object is simply ignored because a simulation system is generated\n"
  | TargetNoTargetDir -> "\n[[TargetNoTargetDir]] target system generation only allowed if a target directory is chosen\n"
  | TargetNotSupported(sys) -> "\n[[TargetNotSupported]] build not supported for target system \'"^sys^"\'\n"
  | ConversionErr(s) -> "\n[[ConversionErr]] literal or constant expression is too large: "^s
  | SuperType(cf, txt, tl) -> "\n[[SuperType]] "^txt^".\nTypes inspected:\n        "^most_gen_type tl^"\n"^cf
  | VectorOp(cf, txt, t) -> "\n[[VectorOp]] Values of vectors used in a "^txt^" are not of type float or double but of type \'"^type2str t^" at "^cf
  | InvalidPre(cl) -> "\n[[InvalidPre]] the pre-operator may only be applied to sensor or signal-objects"^cl
  | MainIgnored -> "\n[[MainIgnored]] the method main is ignored when target is Simulink or Scicos.\n"
  | IllegalField(cf) -> "\n[[IllegalField]] this field definition is illegal.\n"^cf

let msg err_id = raise (Error("Error: "^(text err_id)^"\n"))
let warn err_id = P.primary_ps ("\n\nWARNING: "^(text err_id)^"\n")
let ps err_id = P.primary_ps ("\nError: "^(text err_id)^"\n")
let intern text = raise (Error("[[InternalError]] "^text^"\n"))
