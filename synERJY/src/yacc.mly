%{
       
open Util_parse
open Util
open Ly
open Ast
open Sim_type
open P
%}

%TOKEN <Ast.tcmd_int_or_string> C_EQ
%TOKEN <Ast.tcmd_int_or_string> C_CONC

%TOKEN C_APPLICATION
%TOKEN C_BINARY
%TOKEN C_BLIF
%TOKEN C_BROWSER
%TOKEN C_BUILD
%TOKEN C_C
%TOKEN C_CODE
%TOKEN C_CCLIB
%TOKEN C_CFILE
%TOKEN C_CHECK
%TOKEN C_CLASS
%TOKEN C_CLEAR
%TOKEN C_CONF
%TOKEN C_DATABASE
%TOKEN C_DEBUG
%TOKEN C_EDITOR
%TOKEN C_EXECUTE
%TOKEN C_FILE
%TOKEN C_FONT
%TOKEN C_FORMULA
%TOKEN C_VERILOG
%TOKEN C_GRAPHIC
%TOKEN C_HEIGHT
%TOKEN C_HFILE
%TOKEN C_HOST
%TOKEN C_HOSTOS
%TOKEN C_KIND
%TOKEN C_LEVEL
%TOKEN C_LOAD
%TOKEN C_MAKEFILE
%TOKEN C_MICROSECOND
%TOKEN C_MILLISECOND
%TOKEN C_MC
%TOKEN C_MK
%TOKEN C_MODEL
%TOKEN C_OBJECT
%TOKEN C_PARALLEL
%TOKEN C_PATH
%TOKEN C_PORT
%TOKEN C_PREFIX
%TOKEN C_PRINT
%TOKEN C_PROJECT
%TOKEN C_QUIT
%TOKEN C_SAVE
%TOKEN C_SET
%TOKEN C_SHOW
%TOKEN C_SEFILE
%TOKEN C_SECFILE
%TOKEN C_SIGNAL
%TOKEN C_SIM
%TOKEN C_SIMTOR
%TOKEN C_SIMULINK
%TOKEN C_SCICOS
%TOKEN C_SIZE
%TOKEN C_SYSTEM
%TOKEN C_TARGET
%TOKEN C_TEST
%TOKEN C_TIMESCALE
%TOKEN C_TRACE
%TOKEN C_UPLOADBUTTON
%TOKEN C_VERIFICATION
%TOKEN C_VERILOG
%TOKEN C_VERILOGSIM
%TOKEN C_VISMODEL
%TOKEN C_WEIGHT
%TOKEN C_WIDTH
%TOKEN C_WINDOW
%TOKEN C_WORKSPACE

%TOKEN C_MATLAB
%TOKEN C_NUSMV
%TOKEN C_SCILAB
%TOKEN C_STYLE
%TOKEN C_DIRCTRY

%TOKEN S_BREAK
%TOKEN S_CLASS
%TOKEN S_CONF
%TOKEN S_CONSTR
%TOKEN S_EOL
%TOKEN S_EXP
%TOKEN S_FILENAMES
%TOKEN S_GET
%TOKEN S_GTAG
%TOKEN S_IN
%TOKEN S_INSTANTMINUS
%TOKEN S_EXCEPTION
%TOKEN S_INSTANTPLUS
%TOKEN S_LBL
%TOKEN S_LCL
%TOKEN S_MTAG
%TOKEN S_OUT
%TOKEN S_PRINCIPALSIGNALS
%TOKEN S_REF
%TOKEN S_SESIM
%TOKEN S_TIMING
%TOKEN S_VAL

%TOKEN <string> S_ABORT

%TOKEN <string> S_COMMENT

%TOKEN <string> S_NAME

%TOKEN EOF
%TOKEN GARBAGE

%TOKEN FALSE
%TOKEN TRUE
%TOKEN VOID

%TOKEN RPARENLSQURE

%TOKEN BOOL
%TOKEN BYTE
%TOKEN CHAR
%TOKEN SHORT
%TOKEN UINT16
%TOKEN INT
%TOKEN UINT32
%TOKEN LONG
%TOKEN UINT64
%TOKEN DOUBLE
%TOKEN FLOAT
%TOKEN OBJECT
%TOKEN STRING
%TOKEN TIME
%TOKEN UNSIGNED
%TOKEN NULLTYPE

%TOKEN SENSOR
%TOKEN SIGNAL
%TOKEN DELAYEDSIGNAL

%TOKEN ABSTRACT
%TOKEN ACTIVATE
%TOKEN ACTIVE
%TOKEN ACTL
%TOKEN ALL
%TOKEN ASSERT
%TOKEN AUTOMATON
%TOKEN AWAIT
%TOKEN CONSTRAINT
%TOKEN BLACKBOARD
%TOKEN BREAK
%TOKEN CANCEL
%TOKEN CASE
%TOKEN CLASS
%TOKEN CONTINUE
%TOKEN CTL
%TOKEN DEFAULT
%TOKEN DELTAT
%TOKEN DIAGONAL
%TOKEN DO
%TOKEN DURING
%TOKEN ELSE
%TOKEN EMIT
%TOKEN ENTRY
%TOKEN EXIT
%TOKEN EXTENDS
%TOKEN FAIR
%TOKEN FINAL
%TOKEN FOR
%TOKEN HALT
%TOKEN IF
%TOKEN IMPLEMENTS
%TOKEN IMPORT
%TOKEN INIT
%TOKEN INSTANCEOF
%TOKEN INSTANT
%TOKEN INTERFACE
%TOKEN INTERRUPT
%TOKEN INVARIANT
%TOKEN LOOP
%TOKEN LTL
%TOKEN NATIVE_FROM_C
%TOKEN NATIVE_TO_C
%TOKEN NEW
%TOKEN NEXT
%TOKEN NODE
%TOKEN NOTHING
%TOKEN NULLOBJ
%TOKEN PARAMETER
%TOKEN POST
%TOKEN PRE
%TOKEN PRIVATE
%TOKEN PROTECTED
%TOKEN PROPOSITIONS
%TOKEN PTL
%TOKEN PUBLIC
%TOKEN REACTIVE
%TOKEN RETURN
%TOKEN SCHEDULE
%TOKEN PRECEDENCE
%TOKEN STATE
%TOKEN STATIC
%TOKEN STRICTFP
%TOKEN STRONGLY
%TOKEN SUPER
%TOKEN SUSTAIN
%TOKEN SWITCH
%TOKEN THEN
%TOKEN THIS
%TOKEN THROW
%TOKEN TRANSIENT
%TOKEN UNTIL
%TOKEN UP_SPL
%TOKEN VOLATILE
%TOKEN WHEN
%TOKEN WHILE

%TOKEN MC_AF
%TOKEN MC_AG
%TOKEN MC_AU
%TOKEN MC_AX
%TOKEN MC_EF
%TOKEN MC_EG
%TOKEN MC_EU
%TOKEN MC_EX
%TOKEN MC_X
%TOKEN MC_G
%TOKEN MC_F
%TOKEN MC_Y
%TOKEN MC_Z
%TOKEN MC_H
%TOKEN MC_O
%TOKEN MC_U
%TOKEN MC_V
%TOKEN MC_S
%TOKEN MC_T
%TOKEN MC_HASBEEN
%TOKEN MC_ONCE
%TOKEN MC_PREVIOUS
%TOKEN MC_SINCE

%TOKEN QUESTION
%TOKEN AT
%TOKEN DOLLAR

%TOKEN ARRAY1
%TOKEN ARRAY2
%TOKEN COLON
%TOKEN COMMA
%TOKEN DOTDOT
%TOKEN TRANSP
%TOKEN LCURLY
%TOKEN LDFLOW
%TOKEN LPAREN
%TOKEN LSQURE
%TOKEN PAR_BEGIN
%TOKEN PAR_END
%TOKEN RCURLY
%TOKEN RDFLOW
%TOKEN RPAREN
%TOKEN RSQURE
%TOKEN SEMI

%TOKEN DOT

%TOKEN ADD
%TOKEN AND
%TOKEN ANDCOND
%TOKEN ASSIGN
%TOKEN BAR2
%TOKEN BITCOMPLEMENT
%TOKEN DECR
%TOKEN DIV
%TOKEN EQ
%TOKEN EQUIV
%TOKEN FLOWEQU
%TOKEN GE
%TOKEN GT
%TOKEN ARROW
%TOKEN STATEARROW
%TOKEN INCR
%TOKEN LE
%TOKEN LT
%TOKEN MULT
%TOKEN NE
%TOKEN NOT
%TOKEN OR
%TOKEN OR
%TOKEN MOD
%TOKEN POINTMULT
%TOKEN SUB
%TOKEN XOR

%TOKEN <Ly.tmfid>                          AN_ID
%TOKEN <Ly.tmfid>                          A_BUFFER
%TOKEN <Ly.tmfid>                          A_DERIVED
%TOKEN <Ly.tlbl>                           LABEL
%TOKEN <Ly.tclass>                         A_CLASS
%TOKEN <Ast.ttype * string * int64 option> LITERAL
%TOKEN <Ly.tclass>                         SIMPLE_IN_PAREN
%TOKEN <Ast.ttype>                         SIMPLE_ARRAY1_IN_PAREN
%TOKEN <Ast.ttype>                         SIMPLE_ARRAY2_IN_PAREN
%TOKEN <Ly.tmfid>                          ASSIGN

%nonassoc   MC_AF MC_AG MC_AU MC_AX MC_EF MC_EG MC_EU MC_EX MC_X MC_G MC_F MC_U
            MC_V MC_Y MC_Z MC_H MC_O MC_S MC_T
            MC_HASBEEN MC_ONCE MC_PREVIOUS MC_SINCE UNTIL
%left       EQUIV

%nonassoc   FLOWEQU ASSIGN
%left       ARROW STATEARROW
%right      QUESTION COLON
%left       BAR2
%left       ANDCOND
%left       OR XOR AND
%left       EQ NE
%nonassoc   LT GT LE GE
%left       ADD SUB
%left       MULT DIV MOD POINTMULT
%right      BITCOMPLEMENT NOT
%nonassoc   UPLUS UMINUS
%nonassoc   WHEN
%nonassoc   SIMPLE_IN_PAREN
%nonassoc   SIMPLE_ARRAY_START
%nonassoc   TRANSP
%left       DOT

%start prs_id
%type<Ly.tmfid> prs_id

%start prs_classid
%type<Ly.tclass> prs_classid

%start prs_condtrans
%type<Ast.ttrans> prs_condtrans

%start prs_inittrans
%type<Ast.tstmtl> prs_inittrans

%start prs_trans
%type<Ast.ttrans> prs_trans

%start prs_cond
%type<Ast.tthen_part> prs_cond

%start prs_stateaction
%type<Util_parse.tstatemodif> prs_stateaction

%start prs_import
%type<unit> prs_import

%start prs_class
%type<Ly.tlbl> prs_class

%start prs_cmd
%type<Ast.tcmd_to_do> prs_cmd

%start prs_sim_traceheader
%type<Sim_type.traceheader_t> prs_sim_traceheader

%start prs_sim_class
%type<Sim_type.simclass_t> prs_sim_class

%start prs_sim_signals
%type<Sim_type.signalspec_t list> prs_sim_signals

%start prs_sim_psignals
%type<Sim_type.signalspec_t list> prs_sim_psignals

%start prs_sim_classes
%type<Sim_type.simclass_t list> prs_sim_classes

%start prs_sim_timing
%type<int64> prs_sim_timing

%start prs_sim_targetheader
%type<Sim_type.targetheader_t> prs_sim_targetheader

%start prs_sim_instantplus
%type<Sim_type.instantplus_t> prs_sim_instantplus

%start prs_sim_instantminus
%type<Sim_type.instantminus_t> prs_sim_instantminus

%start prs_sim_instant
%type<Sim_type.instant_t> prs_sim_instant

%start prs_sim_abort
%type<string> prs_sim_abort

%start prs_sim_valopt
%type<Sim_type.simval_t> prs_sim_valopt

%start prs_sim_outmsg
%type<Sim_type.simoutmsg_t> prs_sim_outmsg

%%

/* -------------------------------------------------------------------------- */
prs_class:       ParseFullClass            { $1 } ;
prs_id:          AN_ID EOF                 { $1 } ;
prs_classid:     A_CLASS EOF               { $1 } ;
prs_inittrans:   GraphicInitTrans EOF      { $1 } ;
prs_condtrans:   GraphicTrueFalseTrans EOF { $1 } ;
prs_trans:       GraphicTrans EOF          { $1 } ;
prs_cond:        GraphicCond EOF           { $1 } ;
prs_stateaction: GraphicStateActions       { $1 } ;
prs_cmd:         Command                   { $1 } ;
prs_import:      ImportL                   { () } ;

prs_sim_traceheader:  SimTraceHeader       { $1 } ;
prs_sim_class:   SimClass                  { $1 } ;
prs_sim_signals: SimSignals                { $1 } ;
prs_sim_psignals: SimPrincipalSignals      { $1 } ;
prs_sim_classes: SimClasses                { $1 } ;
prs_sim_timing:  SimTiming                 { $1 } ;
prs_sim_targetheader: SimTargetHeader      { $1 } ;
prs_sim_instantplus:  SimInstantPlus       { $1 } ;
prs_sim_instantminus: SimInstantMinus      { $1 } ;
prs_sim_instant: SimInstant                { $1 } ;
prs_sim_abort:   SimAbort                  { $1 } ;

prs_sim_valopt: SimLiteralOpt              { $1 } ;
prs_sim_outmsg:  SimOutMsg                 { $1 } ;

/* ---- START OF NONTERMINALS ----------------------------------------------- */

/* -------------------------------------------------------------------------- */
ImportL:
    /* empty */              { s.p_blockparse_started <- false }
  | ImportL Import SEMI      { () }
;

Import:
    IMPORT Package A_CLASS   { import_class (List.rev $2) $3 }
  | IMPORT Package MULT      { import_all   (List.rev $2)    }
;

Package:
    /* empty */              { [] }
  | Package AN_ID DOT        { $2::$1 }
;

/* -------------------------------------------------------------------------- */
ParseFullClass:
    EOF                      { raise (Error "end_of_file") }

  | InitClass
    ParseClass
    ExitClass                { $2 }
;

ParseClass:
    ClassHeader
    LCURLY
    ClassBodyDeclL
    RCURLY                   { add_new_lbl () }
;

InitClass:
    /* empty */              { s.p_blockparse_started <- true }
;

ExitClass:
    /* empty */              { s.p_blockparse_started <- false }
;

/* -------------------------------------------------------------------------- */
IgnoreAccess:
    /* empty */              { () }
  | PUBLIC                   { () }
;

AbstractOrFinalClassOpt:
    /* empty */              { None }
  | ABSTRACT                 { Some AbstractClass }
  | FINAL                    { Some FinalClass }
;

ClassOrObject:
  | CLASS                    { s.p_kind = ClassKind }
  | OBJECT                   { s.p_kind = ObjectKind }

ClassHeader:
    IgnoreAccess
    AbstractOrFinalClassOpt
    ClassOrObject A_CLASS
    TypeParamLOpt
    Extends Implements       { s.p_class_id <- $4;
                               s.p_class_kind <- $2;
                               ps "\n  class ";
                               psc s.p_class_id; pF ();
                               ( match $2 with
                                 | Some AbstractClass -> ps " [abstract]"
                                 | Some FinalClass    -> ps " [final]"
                                 | _                  -> ()
                               );
                               s.p_extends <- $6;
                               s.p_implements <- $7 }
  | IgnoreAccess
    INTERFACE A_CLASS
    TypeParamLOpt
    InterfaceExtends         { s.p_class_id <- $3;
                               s.p_class_kind <- Some Interface;
                               ps "\n  interface ";
                               psc s.p_class_id; pF ();
                               s.p_extends <- t_object;
                               s.p_implements <- $5 }
;

TypeParamLOpt:
    /* empty */              { () }
  | LT TypeParamL GT         { List.iter check_typeparm s.p_typparams }
;

TypeParamL:
    TypeParam                { () }
  | TypeParamL COMMA
    TypeParam                { () }
;

TypeParam:
    A_CLASS ConstraintOpt    { add_typeparm $1 $2 }
;

ConstraintOpt:
    /* empty */              { None }
  | IMPLEMENTS A_CLASS       { Some $2 }
;

Extends:
    /* empty */              { t_object }
  | EXTENDS Composite        { $2 }
;

Implements:
    /* empty */              { [] }
  | IMPLEMENTS ParentL       { List.rev $2 }
;

InterfaceExtends:
    /* empty */              { [] }
  | EXTENDS ParentL          { List.rev $2 }
;

ParentL:
    Composite                { [$1] }
  | ParentL COMMA Composite  { $3::$1 }
;

/* -------------------------------------------------------------------------- */
ClassBodyDeclL:
    /* empty */              { () }
  | ClassBodyDeclL
    ClassBodyDecl SEMI       { () }
;

ClassBodyDecl:
    ConstrDecl               { () }
  | SpecDecl                 { () }
  | MethodDecl               { mkp_st_entry ($1 s.p_class_kind s.p_method_kind
                                                s.p_class_id)
                             }
  | NodeDecl                 { () }
  | FieldDecl                { () }
;

/* -------------------------------------------------------------------------- */
ConstrDecl:
    Modifier
    A_CLASS FormalRvL
    PreCondition
    MethodKindConstr
    LCURLY StmtL ActiveOpt
    RCURLY
    PostCondition            { mk_constr_entry $2 $3 $1 $7 $8 $4 $10 }
;

ActiveOpt:
    /* empty */              { enforce_dataclass "reactive classes need an \
                                                  active statement as last \
                                                  statement of its constructor";
                               None }
  | ACTIVE
    ActOn Block ActOff SEMI  { enforce_rctclass "an active statement is \
                                                 illegal for a data class";
                               enforce_datamthd(); 
                               Some $3 }
;

ActOn:
    /* empty */              { s.p_active <- true }
;

ActOff:
    /* empty */              { s.p_active <- false }
;

/* -------------------------------------------------------------------------- */
MethodDecl:
    Modifier
    VoidOrType
    AN_ID
    MethodKindNone
    FormalRvL
    PreCondition
    MethodBody
    PostCondition            { let t,c = $2 in
                                   if List.exists
                                        (fun e->is_sensor_or_signal_typ e.p_type
                                        ) $5
                                      then enforce_rctmthd ()
                                      else ();
                                   mk_method $3 t c $5 $1 $6 $8 $7 }
;

NodeDecl:
    NODE AN_ID
    MethodKindNone
    LPAREN 
    NodeParameterOn
    SignalRvL
    NodeParameterOff
    RPAREN
    FlowContext           { mk_rct_node $2 (List.rev $6) [$9 (add_new_lbl())] }
;

NodeParameterOn:
    /* empty */              { s.p_node_param <- true }
;

NodeParameterOff:
    /* empty */              { s.p_node_param <- false }
;

FieldDecl:
    Modifier VoidOrType FldL { let t,c = $2 in
                                   mk_field_entry (List.rev $3) t c $1 }
;

FldL:
    FldInitOpt               { [$1] }
  | FldL COMMA
    FldInitOpt               { $3::$1 }
;

FldInitOpt:
    AN_ID                    { {mf_name=$1;mf_init=NoInit}        }
  | AN_ID ASSIGN Expr        { {mf_name=$1;mf_init=ExprInit $3}   }
;

/* -------------------------------------------------------------------------- */
MethodKindConstr:
    /* empty */              { s.p_method_kind <- DataMethod;
                               s.p_active      <- false }
;

MethodKindNone:
    /* empty */              { s.p_method_kind <- BttmMethod;
                               s.p_active      <- false }
;

/* -------------------------------------------------------------------------- */
Modifier:
    ModL                     { $1 (m_init ()) }
;

ModL:
    /* empty */              { fun m -> m }
  | ModL MEntry              { fun m -> $2 ($1 m) }
;

MEntry:
    STATIC                   { fun m -> mk_static    m Class; m }
  | VOLATILE                 { fun m -> mk_volatile  m true;  m }
  | ABSTRACT                 { fun m -> mk_abstract  m true;  m }
  | FINAL                    { fun m -> mk_final     m true;  m }
  | PARAMETER                { fun m -> mk_parameter m true;  m }
  | REACTIVE                 { fun m -> mk_reactive  m true;  m }
  | Access                   { fun m -> mk_access    m $1;    m }
  | INTERRUPT
    LPAREN LITERAL RPAREN    { let (t,ls,_) = $3 in
                                   if t=t_string
                                   then (fun m -> mk_interrupt m ls; m)
                                   else ( prs_err "interrupt modifier illegal" )
                             }
  | NATIVE_FROM_C
    LPAREN LITERAL RPAREN    { let (t,ls,_) = $3 in
                                   if t=t_string
                                   then (fun m -> mk_from_C m (NewId ls);m)
                                   else ( prs_err "native import illegal" )
                             }
  | NATIVE_FROM_C            { fun m -> mk_from_C m SameId; m }
  | NATIVE_TO_C
    LPAREN LITERAL RPAREN    { let (t,ls,_) = $3 in
                                   if t=t_string
                                   then (fun m -> mk_to_C m (NewId ls);m)
                                   else ( prs_err "native import illegal" )
                             }
  | NATIVE_TO_C              { fun m -> mk_to_C m SameId; m }
;

Access:
    PUBLIC                             { Public [id_object] }
  | PROTECTED                          { Protected }
  | PRIVATE                            { Private }
  | PUBLIC LCURLY OBJECT RCURLY        { Public [id_object] }
  | PUBLIC LCURLY ClassNeRvL RCURLY    { Public $3 }
;

ClassNeRvL:
    ClassNeL                           { List.rev $1 }
;

ClassNeL:
    A_CLASS                            { [$1] }
  | ClassNeL COMMA A_CLASS             { $3::$1 }
;

/* -------------------------------------------------------------------------- */
PreCondition:
    /* empty */              { [] }
  | PRE ASSERT
    AssertRvL                { $3 }
;

PostCondition:
    /* empty */              { [] }
  | POST ASSERT
    AssertRvL                { $3 }
;

MethodBody:
    /* empty */              { s.p_method_kind <- Abstract; Nobody }
  | Block                    { TextStmtL $1 }
;

/* -------------------------------------------------------------------------- */
FormalRvL:
    LPAREN RPAREN            { [] }
  | LPAREN EntityDclL RPAREN { List.rev $2 }
;

SignalRvL:
    SignalDecl               { [$1] }
  | SignalRvL COMMA
    SignalDecl               { $3 :: $1 }
  | DataType AN_ID           { prs_err "Only sensor and signal types can be \
                                        used for parameters in a node \
                                        declaration." }
;

SignalDecl:
    Signal AN_ID             { let lbl = add_new_lbl ()
                               and t,c = $1 in
                                   { p_lbl=lbl;p_name=$2;p_type=t;p_clock=c }
                             }
;

EntityDclL:
    EntityDecl               { [$1] }
  | EntityDclL COMMA
    EntityDecl               { $3 :: $1 }
;

EntityDecl:
    Type AN_ID               { let lbl = add_new_lbl ()
                               and t,c = $1 in
                                   { p_lbl=lbl;p_name=$2;p_type=t;p_clock=c }
                             }
;

/* -------------------------------------------------------------------------- */
/* type-like data is -not very elegant- a tuple (type,clock option)           */
VoidOrType:
    VOID                     { Null,v_true }
  | Type                     { $1 }
;

Type:
    DataType                 { $1 }
  | Signal                   { $1 }
;

DataType:
    Simple                   { $1      ,v_true }
  | STRING                   { t_string,v_true }
  | Composite ClockOpt       { $1      ,v_true }
  | Array ClockOpt           { $1      ,v_true }
;

Array:
  | ArrayType ARRAY1         { Array($1,DimLen 1,Arbitrary) }
  | ArrayType ARRAY2         { Array($1,Arbitrary,Arbitrary) }
  | ArrayType
     LSQURE Expr RSQURE      { Array($1,DimLen 1,expr2dim $3) }
  | ArrayType LSQURE Expr 
       COMMA Expr RSQURE     { Array($1,expr2dim $3,expr2dim $5) }
;

Simple:
    BOOL                     { t_bool }
  | BYTE                     { t_byte }
  | CHAR                     { t_char }
  | SHORT                    { t_short }
  | UNSIGNED SHORT           { t_uint16 }
  | UINT16                   { t_uint16 }
  | INT                      { t_int }
  | UNSIGNED INT             { t_uint32 }
  | UINT32                   { t_uint32 }
  | LONG                     { t_long }
  | UNSIGNED LONG            { t_uint64 }
  | UINT64                   { t_uint64 }
  | FLOAT                    { t_float }
  | DOUBLE                   { t_double }
  | TIME                     { t_time }
;

Composite:
    OBJECT                   { Typ(id_object,[]) }
  | A_CLASS TypParamOpt      { class2typ $1 $2 }
;

TypParamOpt:
    /* empty */              { [] }
  | LT TypeNeL GT            { List.rev $2 }
;

TypeNeL:
    Type                     { match $1 with
                               | t,c when c.expr = l_true -> [t]
                               | _ -> prs_err "illegal clock definition"
                             }
  | TypeNeL COMMA Type       { match $3 with
                               | t,c when c.expr = l_true -> t::$1
                               | _ -> prs_err "illegal clock definition"
                             }
;

SignalWithoutClock:
    SignalKind SigValueOpt   { class2typ $1 [$2] }
;

Signal:
    SignalKind ClockOpt
    SigValueOpt              { (class2typ $1 [$3],$2) }
;

ClockOpt:
    /* empty */              { v_true }
  | LCURLY Expr RCURLY       { $2 }
;


SignalKind:
    SENSOR                   { id_sensor }
  | SIGNAL                   { id_signal }
  | DELAYEDSIGNAL            { id_delayed }
;

SigValueOpt:
    /* empty */              { Null }
  | LT NULLTYPE GT           { Null }
  | LT Type GT               { match $2 with
                               | t,c when c.expr = l_true  -> t
                               | _ -> prs_err "illegal clock definition"
                             }
;

/* ========================================================================== */
Block:
    LCURLY RawStmtL RCURLY   { reverse_and_letify $2 }
;

StmtL:
    RawStmtL                 { reverse_and_letify $1 }
;

RawStmtL:
    /* empty */              { [] }
  | RawStmtL Stmt SEMI       { $2::$1 }
;

StmtLWOBrace:
    StmtNeL                  { reverse_and_letify $1 }
;

StmtNeL:
    Stmt SEMI                { [$1] }
  | StmtNeL Stmt SEMI        { $2::$1 }
;

Label:
    /* empty */              { None }
  | LABEL                    { enforce_rctmthd_else_err
                                 (* 1. *) "A label is illegal in a non \
                                 reactive class"
                                 (* 2. *) "The method being parsed contains \
                                 statements some of which are restricted to \
                                 occur in data methods and others which are \
                                 restricted to occur in reactive methods only. \
                                 But a method must either be a data or a \
                                 reactive method.\n\
                                 This conflict was detected at: ";
                               mk_label (Ly.lbl2mf $1); Some $1
                             }
;

Stmt:
    ActivateRct              { enforce_rctmthd (); $1 }
  | Label ASSERT AssertRvL   { enforce_datamthd(); AssertStmt(add_sp $1,$3) }
  | AutomatonStmt            { enforce_rctmthd (); $1 }
  | Await                    { enforce_rctmthd (); $1 }
  | Label BREAK              { enforce_datamthd(); Break(add_sp $1,None) }
  | Label BREAK AN_ID        { enforce_datamthd(); Break(add_sp $1,Some $3) }
  | Label CONTINUE           { enforce_datamthd(); Continue(add_sp $1,None) }
  | Label CONTINUE AN_ID     { enforce_datamthd(); Continue(add_sp $1,Some $3) }
  | DoStmt                   { enforce_datamthd(); $1 }
  | Emit                     { enforce_rctmthd (); $1 }
  | Label NOTHING            { (* undecided *)     Nothing(add_sp $1) }
  | ExprStmt                 { (* undecided *)     $1 }
  | ForStmt                  { enforce_datamthd(); $1 }
  | Label HALT               { enforce_rctmthd (); Halt(add_sp $1) }
  | Label LetDecl            { (* see let *)       LetStmt(add_sp $1,$2) }
  | Next                     { enforce_rctmthd (); $1 }
  | NextState                { enforce_rctmthd (); $1 }
  | ParStmt                  { enforce_rctmthd (); $1 }
  | CancelRct                { enforce_rctmthd (); $1 }
  | Label LOOP Block         { enforce_rctmthd (); RctLoop(add_sp $1,$3) }
  | Label RETURN ExprOpt     { enforce_datamthd(); Return(add_sp $1,$3) }
  | Label SCHEDULE ExprParen { (* undecided *)     Schedule(add_sp $1,$3) }
  | SustainStmt              { enforce_rctmthd (); $1 }
  | Switch                   { enforce_datamthd(); $1 }
  | Throw                    { enforce_datamthd(); $1 }
  | While                    { enforce_datamthd(); $1 }
;

/* -------------------------------------------------------------------------- */
ActivateRct:
    Label
    ACTIVATE NextOpt Block
    WHEN ExprParen           { Activate(add_halt_sp 2 $1,$3,$4,$6) }
;
/* -------------------------------------------------------------------------- */
AutomatonStmt:
    Label
    AUTOMATON LCURLY
    RStateNeL RCURLY      { let i,sl = List.partition
                                         (fun s -> s.sname=id_init)
                                         (List.rev $4)                        in
                            let i = match i with
                                    | [i] -> i
                                    | []  -> trans_err "no init state"
                                    | _   -> trans_err "too many init states" in
                            let a = { a_init=i; a_states=sl }                 in
                                TextStM(add_sp $1,a)
                          }
  | Label AUTOMATON
    LITERAL               { let (t,f,_) = $3 in
                                if not( t = t_string)
                                   then prs_err"invalid synERJYcharts file name"
                                   else ();
                            let f = if (Filename.is_relative f)
                                       then Filename.concat (Sys.getcwd()) f
                                       else f                                 in
                            let g = {gstm_file=f;gstm_mf=None}                in
                                let ex_g x = x = g in
                                if not(List.exists ex_g s.p_graphic_stms)
                                then s.p_graphic_stms <- g::s.p_graphic_stms;
                                (* graphics translated only once *)
                                (GraphicStM (add_sp $1,g)) }
;

RStateNeL:
    RctState              { [$1] }
  | RStateNeL RctState    { $2::$1 }
;

RctState:
    STATE AN_ID
    StateActions
    WHEN WhenRvL SEMI     { let _ = chk_when false $5  in   (* ELSE optional *)
                            let a = $3 (stact_init ()) in
                                { slbl=add_halt_sp 1 None;sname=$2;strans=$5;
                                  sentry=stact_entry a;
                                  sduring=stact_during a;
                                  sexit=stact_exit a;
                                  sdo=stact_do a} }
  | INIT Block SEMI       { let t = [Else(nolbl,$2)]      in
                            let _ = chk_when true t in      (* ELSE required *)
                                { slbl=add_new_lbl();sname=id_init;strans=t;
                                  sentry=[];sduring=[];sexit=[];sdo=[] } }
;

GraphicStateActions:
    StateActions          { $1 (stact_init ()) }
;

StateActions:
    /* empty */           { fun m -> m }
  | StateActions Action   { fun m -> $2 ($1 m) }
;

Action:
    ENTRY Block           { fun m -> mk_stact_entry  m $2; m }
  | DURING Block          { fun m -> mk_stact_during m $2; m }
  | DURING FlowContext    { fun m -> mk_stact_during m [$2 (add_new_lbl())]; m }
  | EXIT Block            { fun m -> mk_stact_exit   m $2; m }
  | DO Block              { fun m -> mk_stact_do     m $2; m }
;

/* -------------------------------------------------------------------------- */
Await:
    Label AWAIT NextOpt Expr        { Await(add_halt_sp 2 $1,$3,
                                               [Then(nolbl,$4,[])]) }
  | Label AWAIT NextOpt WHEN WhenRvL{ Await(add_halt_sp 2 $1,$3,$5) }
;

/* -------------------------------------------------------------------------- */
CancelRct:
    Label CANCEL StronglyOpt NextOpt
    Block WHEN WhenRvL               { Cancel(add_sp $1,$3,$4,$5,$7) }
;

/* -------------------------------------------------------------------------- */
StronglyOpt:
    /* empty */              { false }
  | STRONGLY                 { true }
;

NextOpt:
    /* empty */              { false }
  | NEXT                     { true }
;

/* -------------------------------------------------------------------------- */
Next:
    Label NEXT               { Next(add_halt_sp 2 $1) }
;

NextState:
    Label NEXT STATE AN_ID   { NextState(add_sp $1,$4) }
  | Label NEXT STATE EXIT    { NextState(add_sp $1,id_exit) }
;

/* -------------------------------------------------------------------------- */
Emit:
    Label EMIT AN_ID 
    GetOpt EmitVal           { Emit(add_emit_sp 2 $1,StdEmit,$3,$4,$5) }
;

EmitVal:
    /* empty */              { mk_e NullObj }
  | ExprParen                { $1 }
;

/* -------------------------------------------------------------------------- */
Switch:
    Label SWITCH ExprParen
    LCURLY CaseL Dflt RCURLY { let wl = List.rev $5 in
                               let l  = add_sp $1   in
                                   Switch(l,{swexpr=$3;swcase=wl;swdflt=$6}) }
;

CaseL:
    /* empty */              { [] }
  | CaseL Case               { $2::$1 }
;

Case:
    CASE CaseVC StmtL        { {sfrom=$2;sto=None;sstmtl=$3} }
  | CASE CaseV DOTDOT
         CaseVC StmtL        { {sfrom=$2;sto=Some $4;sstmtl=$5} }
;

CaseVC:
    LITERAL COLON            { CaseLiteral $1 }
  | AN_ID COLON              { CaseId $1 }
;

CaseV:
    LITERAL                  { CaseLiteral $1 }
  | AN_ID                    { CaseId $1 }
;

Dflt:
    /* empty */              { [] }
  | DEFAULT COLON StmtL      { $3 }
;

/* -------------------------------------------------------------------------- */
Throw:
    Label THROW ExprParen    { Throw(add_sp $1,$3) }
;

/* -------------------------------------------------------------------------- */
ExprStmt:
    Label Expr               { ExprStmt(add_sp $1,$2) }
;

/* -------------------------------------------------------------------------- */
ParStmt:
    Label PAR_BEGIN
    ParNeL PAR_END           { Par(add_sp $1,List.rev $3) }
;

ParNeL:
    StmtLWOBrace             { [$1] }
  | ParNeL
    BAR2 StmtLWOBrace        { $3::$1 }
;

/* -------------------------------------------------------------------------- */
SustainStmt:
    Label SUSTAIN
    FlowContext           { let sp = add_halt_sp 2 $1 in
                                Sustain(sp,[$3 sp])
                          }
  | Label SUSTAIN Block   { let sp = add_halt_sp 2 $1 in
                                Sustain(sp,$3)
  }
;

/* -------------------------------------------------------------------------- */
FlowContext:
    LDFLOW
    FlowDefL FlowEquL
    RDFLOW                { let dl = List.rev $2
                            and df = List.rev $3 in
                                fun s -> FlowContext(s,{f_dcls=dl;f_equ=df})
                          }
;

FlowDefL:
    /* empty */           { [] }
  | FlowDefL LetDecl SEMI { (LetStmt(add_new_lbl(),$2)) :: $1 }
;

FlowEquL:
    FlowEqu SEMI          { [$1] }
  | FlowEquL FlowEqu SEMI { $2 :: $1 }
;

FlowEqu:
    A_DERIVED
    FLOWEQU Expr          { Emit(add_emit_sp 3 None,StateEq(None),$1,[],$3) }
  | A_DERIVED FLOWEQU
     Expr STATEARROW Expr { Emit(add_emit_sp 3 None,StateEq(Some $3),$1,[],$5) }
  | AN_ID GetOpt
    FLOWEQU Expr          { Emit(add_emit_sp 3 None,FlowEq,$1,$2,$4) }
  | Expr                  { ExprStmt(add_new_lbl (),$1) }
;

GetOpt:
    /* empty */           { [] }
  | LSQURE Get RSQURE     { $2 }

Get:
  | Index                 { [$1] }
  | Index COMMA Index     { [$1;$3] }
;

Index:
    Expr                  { $1 }
  | Expr COLON Expr       { if $1.expr = $3.expr
                            then prs_err "Slices of length 1 are not \
                               supported. Please use one of the bounds \
                               as index.\n"
                            else let l,u = expr2index $1,expr2index $3 in
                                 mk_e (Slice{lower=l;upper=u}) }
;

/* -------------------------------------------------------------------------- */
GraphicTrueFalseTrans:
    WHEN GraphicTF
    OptActionL            { {tr_priority=1;tr_trans=Then(nolbl,$2,$3)} }
;

GraphicTF:
    LPAREN TRUE  RPAREN   { v_true }
  | LPAREN FALSE RPAREN   { v_false }
;

GraphicInitTrans:
    /* empty */           { [] }
  | Block                 { $1 }
;

GraphicTrans:
    GraphicPrioOpt WHEN 
    ExprParen OptActionL  { {tr_priority=$1;tr_trans=Then(nolbl,$3,$4)} }
;

GraphicPrioOpt:
    /* empty */           { 1 }
  | LITERAL COLON         { let (t,_,v) = $1 in
                                if t = t_cuint7
                                   then lit2int $1
                                   else prs_err "invalid priority" }
;

OptActionL:
    /* empty */           { [] }
  | Block                 { $1 }
;

GraphicCond:
    Expr                  { Then(nolbl,$1,[]) }
;

/* -------------------------------------------------------------------------- */
LetDecl:
    Type AN_ID LetInit    { let t,c = $1 in mk_let_data $2 t c $3 }
;

LetInit:
    /* empty */           { None }
  | ASSIGN Expr           { Some $2 }
;

/* -------------------------------------------------------------------------- */
ForStmt:
    Label
    FOR LPAREN
    ForInit SEMI
    ExprOpt SEMI
    ExprOpt RPAREN
    Block                 { let sp = add_sp $1 in
                                $4 sp {forinit=None;fortest=$6;
                                       forupd=$8;forstmtl=$10}
                          }
;

ForInit:
    /* empty */           { fun l f -> ForStmt(l,f) }
  | Expr                  { fun l f -> ForStmt(l,{f with forinit=Some $1})}
  | LetDecl               { fun l f -> let f = ForStmt(l,f) in
                                           LetStmt(l,{$1 with letil=[f]})
                          }
;

/* -------------------------------------------------------------------------- */

DoStmt:
    Label DO Block
    WHILE ExprParen       { DoStmt(add_sp $1,$5,$3) }
;

While:
    Label WHILE ExprParen
    Block                 { While(add_sp $1,$3,$4) }
;

/* -------------------------------------------------------------------------- */
Literal:
    LITERAL                    { mk_e (Literal $1) }
  | LCURLY LitExprNeL RCURLY   { mk_arraylit (List.rev $2) }
  | DIAGONAL LPAREN
             LITERAL RPAREN    { mk_e_pre id_op_diagonal (mk_e (Literal $3)) }
  | ALL LPAREN LITERAL RPAREN  { mk_e_pre id_op_all (mk_e (Literal $3)) }
;

LitExprNeL:
    Expr                       { [$1] }
  | LitExprNeL COMMA Expr      { $3 :: $1 }
;

/* -------------------------------------------------------------------------- */
ActualRvLOpt:
    /* empty */                { None }
  | ActualRvL                  { Some $1 }
;

ActualRvL:
    LPAREN RPAREN              { [] }
  | LPAREN ActualNeL RPAREN    { (List.rev $2) }
;

ActualNeL:
    Expr                       { [$1] }
  | ActualNeL COMMA Expr       { $3::$1 }
;

/* -------------------------------------------------------------------------- */
ExprParen:
    LPAREN LABEL Expr RPAREN { mk_label (Ly.lbl2mf $2);
                               {$3 with elbl=add_sp (Some $2)}
                             }
  | LPAREN Expr RPAREN       { $2 }
;

/* -------------------------------------------------------------------------- */
ExprParenExpr:
    LPAREN LABEL Expr RPAREN { mk_label (Ly.lbl2mf $2);
                               {$3 with elbl=add_sp (Some $2)}
                             }
  | LPAREN LABEL Expr
    RPARENLSQURE Get RSQURE 
                             { mk_label (Ly.lbl2mf $2);
                               { (mk_edot $3 (mk_ecall id_op_get (Some $5)))
                                 with elbl=add_sp (Some $2)}
                             }
  | LPAREN Expr RPAREN       { $2 }
  | LPAREN Expr
    RPARENLSQURE Get RSQURE { mk_edot $2 (mk_ecall id_op_get (Some $4)) }
    
;

ExprOpt:
    /* empty */              { None }
  | Expr                     { Some $1 }
;

/* -------------------------------------------------------------------------- */
Cast:
    Composite                { $1 }
  | Array                    { prs_err array_cast_err }
;

Primary:
    Primary DOT Call         { mk_edot $1 $3 }
  | Call                     { $1 }
  | PrimaryNoNew             { $1 }
  | PrimaryNoNew
     LSQURE Get RSQURE       { mk_edot $1 (mk_ecall id_op_get (Some $3)) }
  | ExprParenExpr            { $1 }
  | LPAREN
    LPAREN Cast RPAREN
    Expr RPAREN              { mk_e (Cast($3,$5)) }
  | LPAREN
      SIMPLE_ARRAY1_IN_PAREN
	 Expr RPAREN             { prs_err array_cast_err }
  | LPAREN
      SIMPLE_ARRAY2_IN_PAREN
     Expr RPAREN             { prs_err array_cast_err }
  | SIMPLE_IN_PAREN Primary  { let e = $2 in
                                   if is_literal e
                                      then mk_extended_lit $1 e.expr
                                      else mk_e_pre (simple2cast $1) e
                             }
;

Call:
    AN_ID ActualRvLOpt       { mk_ecall $1 $2 }
  | AN_ID LSQURE Get RSQURE  { mk_edot (mk_ecall $1 None) 
                                       (mk_ecall id_op_get (Some $3)) }
;

/* -------------------------------------------------------------------------- */
PrimaryNoNew:
    PrimaryData              { $1 }
  | PrimaryReactive          { enforce_rctmthd (); $1 }

  | TRUE                     { v_true (* all four are Literals *) }
  | FALSE                    { v_false }
  | NULLOBJ                  { mk_e NullObj }
  | Literal                  { $1 }
;

PrimaryData:
    THIS                     { mk_e (This) }
  | SUPER DOT AN_ID
    ActualRvL                { mk_e (Super($3,$4)) }
  | THIS ActualRvL           { mk_e (ThisConstr $2) }
  | SUPER ActualRvL          { mk_e (SuperConstr $2) }
  | ClassCall                { $1 }
  | INSTANT LPAREN RPAREN    { mk_e Instant }
;

PrimaryReactive:
  | A_BUFFER                 { mk_esig $1 id_op_dotdot }
  | QUESTION AN_ID           { mk_esig $2 id_op_present }
  | DOLLAR AN_ID             { mk_esig $2 id_op_value }
  | AT AN_ID                 { mk_esig $2 id_op_timestamp }

  | DELTAT                   { mk_e (DeltaT) }
  | DOLLAR DELTAT            { mk_bssig (DeltaT) id_op_value }

  | UP_SPL ExprParen         { mk_e_pre id_op_upspl $2 }
  | PRE ExprParen            { mk_e_pre id_op_pre   $2 }
;

/* -------------------------------------------------------------------------- */
PrimaryNew:
    NEW Composite ActualRvL  { mk_e (New($2,$3)) }
  | NEW Composite
    LPAREN RPAREN
    LCURLY
    PushMethData
    AnonMethL
    PopMethData
    RCURLY                   { let lbl = add_new_lbl () in
                                   mk_anon_class__rtn_new $2 (List.rev $7) lbl }

  | NEW ArrayType
    LSQURE Expr RSQURE       { try let d = expr2dim $4 in
                                   mk_e (New( Array($2,DimLen 1,d),[$4]))
                               with Parse _ ->
                                   mk_e (New(Array($2,DimLen 1,Arbitrary),
                                        [$4]))
                             }
  | NEW ArrayType
    LSQURE Expr COMMA
    Expr RSQURE              { try let d1 = expr2dim $4
                                   and d2 = expr2dim $6 in
                                   mk_e (New(Array($2,d1,d2),[$4;$6]))
                               with Parse _ ->
                                   mk_e (New(Array($2,Arbitrary,Arbitrary),
                                         [$4;$6]))
                             }
  | NEW SignalWithoutClock
    ActualRvL                { mk_e (New($2,$3)) }
;

PushMethData:
    /* empty */              { push_meth_data () }
;

PopMethData:
    /* empty */              { pop_meth_data () }
;

AnonMethL:
    /* empty */              { [] }
  | AnonMethL
    MethodDecl SEMI          { $2::$1 }
;

ArrayType:
    Simple                   { $1 }
  | STRING                   { t_string }
  | A_CLASS                  { Typ($1,[]) }
;

/* -------------------------------------------------------------------------- */
ClassCall:
    ClassCallPermit DOT
    AN_ID ActualRvLOpt       { mk_e (ClassCall($1,$3,$4)) }
;

ClassCallPermit:
    A_CLASS                  { $1 }
  | TIME                     { id_time }
;

/* -------------------------------------------------------------------------- */
PrefixPostfixExpr:
    Primary                  { $1 }
  | PrimaryNew               { $1 }
  | Primary
    LCURLY Expr RCURLY       { mk_e_inf id_op_downspl $1 $3 }
  | IncrDecr                 { $1 }
;

IncrDecr:
    AN_ID INCR               { mk_e (IncrDecr($1,id_op_postfix_incr)) }
  | AN_ID DECR               { mk_e (IncrDecr($1,id_op_postfix_decr)) }
  | INCR AN_ID               { mk_e (IncrDecr($2,id_op_prefix_incr )) }
  | DECR AN_ID               { mk_e (IncrDecr($2,id_op_prefix_decr )) }
;

/* -------------------------------------------------------------------------- */
Expr:
    PrefixPostfixExpr        { $1 }

  | If                       { mk_e $1 }

  | Expr ASSIGN    Expr      { mk_e_assign $1 $2 $3 }

  | Expr ARROW     Expr      { mk_e_inf id_op_fby     $1 $3 }

  | Expr BAR2      Expr      { mk_e_inf id_op_log_or  $1 $3 }
  | Expr ANDCOND   Expr      { mk_e_inf id_op_log_and $1 $3 }

  | Expr OR        Expr      { mk_e_inf id_op_bit_or  $1 $3 }
  | Expr XOR       Expr      { mk_e_inf id_op_xor     $1 $3 }
  | Expr AND       Expr      { mk_e_inf id_op_bit_and $1 $3 }

  | Expr EQ        Expr      { mk_e_inf id_op_equal     $1 $3 }
  | Expr NE        Expr      { mk_e_inf id_op_not_equal $1 $3 }

  | Expr LT        Expr      { mk_e_inf id_op_lt $1 $3 }
  | Expr GT        Expr      { mk_e_inf id_op_gt $1 $3 }
  | Expr LE        Expr      { mk_e_inf id_op_le $1 $3 }
  | Expr GE        Expr      { mk_e_inf id_op_ge $1 $3 }

  | Expr LT LT     Expr      { mk_e_inf id_op_leftshift   $1 $4 }
  | Expr GT GT     Expr      { mk_e_inf id_op_rightshift  $1 $4 }
  | Expr GT GT GT  Expr      { mk_e_inf id_op_rightshift0 $1 $5 }

  | Expr ADD       Expr      { mk_e_inf id_op_add $1 $3 }
  | Expr SUB       Expr      { mk_e_inf id_op_sub $1 $3 }

  | Expr MULT      Expr      { mk_e_inf id_op_mult        $1 $3 }
  | Expr DIV       Expr      { mk_e_inf id_op_div         $1 $3 }
  | Expr MOD       Expr      { mk_e_inf id_op_mod         $1 $3 }

  | Expr TRANSP              { mk_e_pre id_op_transp      $1    }
  | Expr POINTMULT Expr      { mk_e_inf id_op_pointmult   $1 $3 }

  | SUB  Expr %prec UMINUS   { mk_e_pre id_op_minus $2      }
  | ADD  Expr %prec UPLUS    { mk_e_pre id_op_plus  $2      }
  | NOT  Expr                { mk_e_pre id_op_not   $2      }
  | BITCOMPLEMENT  Expr      { mk_e_pre id_op_complement $2 }

  | Expr WHEN      Expr      { mk_e_inf id_op_downspl $1 $3 }
;

/* -------------------------------------------------------------------------- */
If:
    IF ThenRvL               { If($2) }
  | Expr QUESTION Expr      
    COLON Expr               { let lbl = add_new_lbl () in
                               let t  = ExprStmt(lbl,$3)
                               and e  = ExprStmt(lbl,$5) in
                                   If([Then(nolbl,$1,[t]);Else(nolbl,[e])])
                             }
;

/* -------------------------------------------------------------------------- */
ThenRvL:
    ThenL                    { List.rev $1 }
;

ThenL:
    ThenPart                 { [$1] }
  | ThenL ELSE IF ThenPart   { $4::$1 }
  | ThenL ElsePart           { $2::$1 }
;

ThenPart:
    ExprParen Block          { Then(nolbl,$1,$2) }
;

ElsePart:
    ELSE Block               { Else(nolbl,$2) }
;


/* -------------------------------------------------------------------------- */
WhenRvL:
    WhenL                    { List.rev $1 }
;

WhenL:
    WhenPart                 { [$1] }
  | WhenL ELSE WHEN WhenPart { $4::$1 }
  | WhenL ElsePart           { $2::$1 }
;

WhenPart:
    ExprParen Block          { Then(nolbl,$1,$2) }
  | ExprParen                { Then(nolbl,$1,[]) }
;

/* -------------------------------------------------------------------------- */
SpecDecl:
    PRECEDENCE LCURLY
    PrecedenceL RCURLY       { s.p_specs <- s.p_specs @ List.rev $3 }
  | CONSTRAINT LCURLY
    VerifConstraintRvL RCURLY{ s.p_axioms <- s.p_axioms @ List.rev $3 }
  | PROPOSITIONS LCURLY
    VerifCondRvL RCURLY      { s.p_props <- s.p_props @ List.rev $3 }
  | INVARIANT AssertRvL      { s.p_specs <- s.p_specs @ $2 }
;

/* -------------------------------------------------------------------------- */
PrecedenceL:
    /* empty */              { [] }
  | PrecedenceL
    Precedence SEMI          { $2::$1 }
;

VerifConstraintRvL:
    /* empty */              { [] }
  | VerifConstraintRvL VerifConstraint
    SEMI                     { $2::$1 }
;

VerifCondRvL:
    /* empty */              { [] }
  | VerifCondRvL VerifCond
    SEMI                     { $2::$1 }
;

/* -------------------------------------------------------------------------- */
Precedence:
   ClassPrecedenceL          { ClassPrec(List.rev $1)   }
 | FOR AN_ID COLON
   ObjNameOrThis LT
   PrecedenceOnObjNeL        { ObjPrec($2,$4 :: List.rev $6) }
;

ClassPrecedenceL:
     SignOrLabel             { [$1] }
   | ClassPrecedenceL LT
     SignOrLabel             { (List.rev $3)::$1 }
;

SignOrLabel:
    Signature                { [$1] }
  | Signature MULT           { [$1;$1] }
  | LabelNeL                 { [LabelPrec (List.rev $1)] }
;

Signature:
    AN_ID                    { RhsFieldPrec ($1) }
  | LPAREN AN_ID ASSIGN
    RPAREN                   { LhsFieldPrec ($2) }
  | AN_ID
    LPAREN RPAREN            { MethodPrec ($1,[]) }
  | AN_ID
    LPAREN TypeNeL RPAREN    { MethodPrec ($1,List.rev $3) }
  | AN_ID SIMPLE_IN_PAREN    { MethodPrec ($1,[Simple $2]) }
                               /* for type casts with primitive types the token
                                  SIMPLE_IN_PAREN had to be introduced to parse
                                  '(<simple>) as prefix operator.
                                  Unpleasant consequence is, that
                                  '(<simple>)' is parsed VERY different to
                                  '( <simple>)' */
;

LabelNeL:
    LABEL                    { [$1] }
  | LabelNeL LABEL           { $2 :: $1 }
;

PrecedenceOnObjNeL:
    ObjNameOrThis            { [$1] }
  | PrecedenceOnObjNeL
    LT ObjNameOrThis         { $3::$1 }
;

ObjNameOrThis:
    THIS                     { This          }
  | AN_ID                    { Call($1,None) }
;

/* -------------------------------------------------------------------------- */
AssertRvL:
    LCURLY AssertL RCURLY    { List.rev $2 }
;

AssertL:
    /* empty */              { [] }
  | AssertL Assertion SEMI   { $2::$1 }
;

Assertion:
    ExprParen ThenOrElse
    THROW ExprParen          { Assertion($1,$2,$4) }
  | LITERAL                  { Comment(lit2string $1) }
;

/* -------------------------------------------------------------------------- */
VerifConstraint:
    AN_ID COLON GT
      Literal                { ValConstraint(add_new_lbl(),$1,
                                             {lower=v_zero;upper=$4}) }
  | AT AN_ID COLON GT
      Literal                { ValConstraint(add_new_lbl(),$2,
                                             {lower=v_zero;upper=$5}) }
  | ACtl STATEARROW ACtl     { ACtl(add_new_lbl(),$3,$1) }
;

/* -------------------------------------------------------------------------- */
VerifCond:
  | CtlVerifCond             { $1 }
  | LtlVerifCond             { $1 }
  | PtlVerifCond             { $1 }
;

/* -------------------------------------------------------------------------- */
CtlVerifCond:
    LABEL CTL Ctl            { mk_label (Ly.lbl2mf $1);
                               Ctl($1,$3,CtlConst(v_true)) }
  | LABEL CTL Ctl FAIR Ctl   { mk_label (Ly.lbl2mf $1);
                               Ctl($1,$3,$5) }
;

PCtl:
  | QUESTION AN_ID           { CtlSig($2) }
  | DOLLAR AN_ID             { CtlSigVal($2) }
  | NOT PCtl                 { CtlNot($2) }
  | PCtl XOR PCtl            { CtlXor($1,$3) }
  | PCtl EQUIV PCtl          { CtlEquiv($1,$3) }

ACtl:
    PCtl                     { $1 }
  | MC_AX ACtl               { CtlAx($2) }
  | MC_AF ACtl               { CtlAf($2) }
  | MC_AG ACtl               { CtlAg($2) }
  | MC_AU LSQURE ACtl
    UNTIL ACtl RSQURE        { CtlAu($3,$5) }
  | LPAREN ACtl RPAREN       { $2 }
  | ACtl AND ACtl            { CtlAnd($1,$3) }
  | ACtl OR ACtl             { CtlOr($1,$3) }
  | PCtl ARROW ACtl          { CtlImplies($1,$3) }

Ctl:
    MC_AX Ctl                { CtlAx($2) }
  | MC_AF Ctl                { CtlAf($2) }
  | MC_AG Ctl                { CtlAg($2) }
  | MC_EX Ctl                { CtlEx($2) }
  | MC_EF Ctl                { CtlEf($2) }
  | MC_EG Ctl                { CtlEg($2) }
  | MC_AU LSQURE Ctl
    UNTIL Ctl RSQURE         { CtlAu($3,$5) }
  | MC_EU LSQURE Ctl
    UNTIL Ctl RSQURE         { CtlEu($3,$5) }
  | LPAREN Ctl RPAREN        { $2 }

  | TRUE                     { CtlConst(v_true) }
  | FALSE                    { CtlConst(v_false) }
  | QUESTION AN_ID           { CtlSig($2) }
  | DOLLAR AN_ID             { CtlSigVal($2) }
  | NOT Ctl                  { CtlNot($2) }
  | Ctl AND Ctl              { CtlAnd($1,$3) }
  | Ctl OR Ctl               { CtlOr($1,$3) }
  | Ctl XOR Ctl              { CtlXor($1,$3) }
  | Ctl ARROW Ctl            { CtlImplies($1,$3) }
  | Ctl EQUIV Ctl            { CtlEquiv($1,$3) }
;

/* -------------------------------------------------------------------------- */
LtlVerifCond:
    LABEL LTL Ltl            { mk_label (Ly.lbl2mf $1); Ltl($1,$3) }
;

Ltl:
    MC_X Ltl                 { LtlX($2) }
  | MC_G Ltl                 { LtlG($2) }
  | MC_F Ltl                 { LtlF($2) }
  | Ltl MC_U Ltl             { LtlU($1,$3) }
  | Ltl MC_V Ltl             { LtlV($1,$3) }
  | MC_Y Ltl                 { LtlY($2) }
  | MC_Z Ltl                 { LtlZ($2) }
  | MC_H Ltl                 { LtlH($2) }
  | MC_O Ltl                 { LtlO($2) }
  | Ltl MC_S Ltl             { LtlS($1,$3) }
  | Ltl MC_T Ltl             { LtlT($1,$3) }
  | LPAREN Ltl RPAREN        { $2 }

  | TRUE                     { LtlConst(v_true) }
  | FALSE                    { LtlConst(v_false) }
  | QUESTION AN_ID           { LtlSig($2) }
  | DOLLAR AN_ID             { LtlSigVal($2) }
  | NOT Ltl                  { LtlNot($2) }
  | Ltl AND Ltl              { LtlAnd($1,$3) }
  | Ltl OR Ltl               { LtlOr($1,$3) }
  | Ltl XOR Ltl              { LtlXor($1,$3) }
  | Ltl ARROW Ltl            { LtlImplies($1,$3) }
  | Ltl EQUIV Ltl            { LtlEquiv($1,$3) }
;

/* -------------------------------------------------------------------------- */
PtlVerifCond:
    LABEL PTL Ptl            { mk_label (Ly.lbl2mf $1); Ptl($1,$3) }
;

Ptl:
    MC_PREVIOUS Ptl          { PtlPrevious($2) }
  | Ptl MC_SINCE Ptl         { PtlSince($1,$3) }
  | MC_ONCE Ptl              { PtlOnce($2) }
  | MC_HASBEEN Ptl           { PtlHasBeen($2) }
  | LPAREN Ptl RPAREN        { $2 }

  | TRUE                     { PtlConst(v_true) }
  | FALSE                    { PtlConst(v_false) }
  | QUESTION AN_ID           { PtlSig($2) }
  | NOT Ptl                  { PtlNot($2) }
  | Ptl AND Ptl              { PtlAnd($1,$3) }
  | Ptl OR Ptl               { PtlOr($1,$3) }
  | Ptl ARROW Ptl            { PtlImplies($1,$3) }
  | Ptl EQUIV Ptl            { PtlEquiv($1,$3) }
;

/* -------------------------------------------------------------------------- */
ThenOrElse:
    THEN                     { true }
  | ELSE                     { false }
;

/* -------------------------------------------------------------------------- */
Command:
    SetCommand SEMI               { CmdUnit }
  | SetFont C_WEIGHT C_EQ SEMI    { $1 font_weight $3; CmdRedisplay }
  | SetFont C_SIZE   C_EQ SEMI    { $1 font_size   $3; CmdRedisplay }
  | BuildCommand SEMI             { $1 }
  | MiscCommand SEMI              { $1 }
  | EOF                           { CmdEof }
;

SetCommand:
    C_SET C_GRAPHIC C_WIDTH C_EQ  { se.ge_width   <- cmd_int $4 }
  | C_SET C_GRAPHIC C_HEIGHT C_EQ { se.ge_height  <- cmd_int $4 }
  | C_SET C_PRINT C_SIZE C_EQ     { se.print_format  <- cmd_prtsize $4 }
  | C_SET C_FILE C_PREFIX C_EQ    { se.file_prefix   <- cmd_fprefix $4 }
  | C_SET C_EDITOR C_EQ           { se.editor     <- cmd_string $3 }
  | C_SET C_GRAPHIC C_FILE C_EQ   { se.sc_file    <- cmd_string $4 }
  | C_SET C_LOAD C_FILE C_EQ      { se.se_file    <- cmd_string $4 }
  | C_SET C_BINARY C_FILE C_EQ    { se.simbinary  <- cmd_string $4 }
  | C_SET C_MC C_CLASS C_EQ       { se.mc_class   <- cmd_string $4 }
  | C_SET C_MC C_FORMULA C_EQ     { se.mc_name    <- cmd_string $4 }
  | C_SET C_WORKSPACE C_EQ        { let d = cmd_string $3 in
                                    if Sys.file_exists d
                                       then se.workspace  <- d }
  | C_CLEAR C_WORKSPACE           { se.workspace  <- "" }
  | C_SET C_MATLAB C_DIRCTRY C_EQ { let d = cmd_string $4 in
                                    if Sys.file_exists d
                                       then se.matlab_dir  <- d }
  | C_CLEAR C_MATLAB C_DIRCTRY    { se.matlab_dir  <- "" }
  | C_SET C_SCILAB C_DIRCTRY C_EQ { let d = cmd_string $4 in
                                    if Sys.file_exists d
                                       then se.scilab_dir  <- d }
  | C_CLEAR C_SCILAB C_DIRCTRY    { se.scilab_dir  <- "" }
  | C_SET C_NUSMV C_DIRCTRY C_EQ  { let d = cmd_string $4 in
                                    if Sys.file_exists d
                                       then se.nu_smv_dir  <-d }
  | C_CLEAR C_NUSMV C_DIRCTRY     { se.nu_smv_dir  <- "" }
  | C_SET C_CONF C_CLASS C_EQ     { let cid = type2id (cmd_conf $4) in
                                    try  let _ = Hashtbl.find se.classtab cid in
                                         se.conf_class <- cmd_conf $4
                                    with _ -> ()
                                  }
  | C_TARGET C_SIM                { se.target_sys <- Simulation }
  | C_TARGET C_HOST               { se.target_sys <- Host }
  | C_TARGET C_SIMULINK           { se.target_sys <- Simulink }
  | C_TARGET C_SCICOS             { se.target_sys <- Scicos }
  | C_TARGET C_VERIFICATION       { se.target_sys <- Verification }
  | C_C C_TARGET C_EQ             { se.target_sys <- Platform (cmd_string $3) }
  | C_TARGET C_VERILOGSIM         { se.target_sys <- VerilogSimulation }
  | C_VERILOG C_TARGET C_EQ       { se.target_sys <- Verilog (cmd_string $3) }
  | C_SET C_DEBUG C_LEVEL C_EQ    { let s = String.lowercase (cmd_string $4) in
                                        se.debug_level <- Some s }
  | C_CLEAR C_DEBUG C_LEVEL       { se.debug_level <- None }

  | C_SET C_SEFILE C_EQ           { se.sefiles <- [cmd_string $3] }
  | C_SET C_SEFILE C_CONC         { se.sefiles <- cmd_string $3:: se.sefiles }
  | C_CLEAR C_SEFILE              { se.sefiles <- [] }
  | C_SET C_SECFILE C_EQ          { se.secfiles <- [cmd_string $3] }
  | C_SET C_SECFILE C_CONC        { se.secfiles <- cmd_string $3 :: se.secfiles}
  | C_CLEAR C_SECFILE             { se.secfiles <- [] }
  | C_SET C_TRACE C_FILE C_EQ     { se.trace_files <- [cmd_string $4] }
  | C_SET C_TRACE C_FILE C_CONC   { se.trace_files <- cmd_string $4 :: 
                                                               se.trace_files}
  | C_CLEAR C_SCICOS C_MODEL      { se.scicos_models <- [] }
  | C_LOAD C_SCICOS C_MODEL C_EQ  { se.scicos_models <- [cmd_string $4] }
  | C_LOAD C_SCICOS C_MODEL C_CONC{ se.scicos_models <- cmd_string $4 :: 
                                                               se.scicos_models}
  | C_CLEAR C_TRACE C_FILE        { se.scicos_models <- [] }
  | C_SET C_CFILE C_EQ            { se.cfiles <- [cmd_string $3] }
  | C_SET C_CFILE C_CONC          { se.cfiles <- cmd_string $3 :: se.cfiles }
  | C_CLEAR C_CFILE               { se.cfiles <- [] }
  | C_SET C_HFILE C_EQ            { se.hfiles <- [cmd_string $3] }
  | C_SET C_HFILE C_CONC          { se.hfiles <- cmd_string $3 :: se.hfiles }
  | C_CLEAR C_HFILE               { se.hfiles <- [] }
  | C_SET C_CCLIB C_EQ            { se.cclibs <- [cmd_string $3] }
  | C_SET C_CCLIB C_CONC          { se.cclibs <- cmd_string $3 :: se.cclibs }
  | C_CLEAR C_CCLIB               { se.cclibs <- [] }
  | C_SET C_UPLOADBUTTON C_EQ     { se.uploadbutton <- 
                                        match cmd_string $3 with
                                        | "normal"   -> "normal"
                                        | "active"   -> "active"
                                        | "disabled" -> "disabled"
                                        | _ ->  prs_err "not a valid state \
                                                for the upload/run button"
                                  }
  | C_SET C_VERILOG C_FILE C_EQ   { se.vfiles <- [cmd_string $4] }
  | C_SET C_VERILOG C_FILE C_CONC { se.vfiles <- cmd_string $4 :: se.vfiles }
  | C_CLEAR C_VERILOG C_FILE      { se.vfiles <- [] }


  | C_SET C_HOSTOS  C_EQ          { se.hostos <- cmd_string $3 }
  | C_SET C_TIMESCALE  C_EQ       { se.timescale <- cmd_int $3 }
  | C_SET C_PROJECT C_PATH C_EQ   { se.projectpath <- cmd_string $4 }
  | C_SET C_PROJECT C_KIND C_EQ   { se.project_kind <-
                                        match cmd_string $4 with
                                        | "C-code"  -> CPrj
                                        | "Verilog" -> VerilogPrj
                                        | _ -> prs_err "Not a valid kind \
                                                        of a project"
                                  }
;

SetFont:
    C_SET C_FONT                  { fun f p -> se.se_font  <- f se.se_font  p }
  | C_SET C_GRAPHIC C_FONT        { fun f p -> se.ge_font  <- f se.ge_font  p }
  | C_SET C_SIMTOR  C_FONT        { fun f p -> se.sim_font <- f se.sim_font p }

BuildCommand:
    C_MK C_C                      { CmdMkCode }
  | C_MK C_BUILD                  { CmdMkBuild }
  | C_MK C_BINARY                 { CmdMkTestBinary }
  | C_MK C_TEST                   { CmdMkTest }
/*  | C_MK C_CLASS C_VISMODEL       { CmdVisClass }
  | C_MK C_APPLICATION C_VISMODEL { CmdVisAppl }
  | C_MK C_CLASS C_FORMULA        { CmdFormClass }
  | C_MK C_APPLICATION C_FORMULA  { CmdFormAppl }
  | C_SAVE C_APPLICATION C_BLIF   { CmdBlifAppl } */
;

MiscCommand:
    C_CLEAR C_DATABASE            { CmdReset }
  | C_CLEAR C_WINDOW              { CmdClearwdw }
  | C_LOAD C_FILE C_EQ            { CmdLoadFile (cmd_string $3) }
  | C_LOAD C_TRACE C_FILE C_EQ    { CmdLoadTraceFile (cmd_string $4) }
  | C_CHECK C_CONF                { CmdSetConfClass None }
  | C_CHECK C_CONF C_EQ           { CmdSetConfClass (Some (cmd_string $3)) }

  | C_LOAD C_CONF C_FILE C_EQ     { CmdLoadSimConf (cmd_string $4) }
  | C_SAVE C_CONF C_FILE C_EQ     { CmdSaveSimConf (cmd_string $4) }
  | C_SHOW C_OBJECT C_BROWSER     { CmdObjectBrowser }
  | C_SHOW C_TRACE C_BROWSER      { CmdTraceBrowser }
  | C_SHOW C_CONF C_SIGNAL C_EQ   { let s = match cmd_string $4 with
                                            | "all"     -> AllSig
                                            | "visible"
                                            | "inout"   -> InOutSig
                                            | _         -> raise Not_found in
                                        CmdConfSignal s
                                  }

  | C_EXECUTE C_SIM               { CmdExecTool Sim }
  | C_EXECUTE C_GRAPHIC C_EDITOR  { CmdExecTool Ge }
  | C_EXECUTE C_EDITOR            { CmdExecTool Edit }
  | C_QUIT                        { CmdQuit }

  | C_PRINT C_EQ                  { CmdPrint (cmd_string $2) }
  | C_SET C_PARALLEL C_PORT C_EQ  { CmdSetParPort (cmd_string $4) }
  | C_SET C_CODE C_STYLE C_EQ     { CmdSetCodeStyle (cmd_string $4) }

/* -------------------------------------------------------------------------- */
SimClassArgsOpt:
    /* empty */                     { [] } /* no type args */
  | LT SimClassArgL GT              { $2 }
;

SimClassArgL:
  | SimClassName                    { [$1] }
  | SimClassName COMMA SimClassArgL { $1::$3 }
;

SimClassName:
    S_NAME SimClassArgsOpt          { $1 ^
                                       (match $2 with
                                        | [] -> ""
                                        | l  -> "<"^(String.concat "," $2)^">"
                                       )
                                    }
;

SimTime:
    LITERAL    { match $1 with
                 | (_,_,Some v) -> v
                 | (_,s,None )  -> (try Int64.of_string s with
                                     _ -> Sim_error.intern
                                           ("SimTime at"^string_of_int  
                                                 Sim_type.sim.parsebuf.pb_line)
                                   )
               }
;

SimOptTime:
    /* empty */                     { Int64.zero }  /* no time */
  | SimTime                         { $1 }
;

SimNamedLiteral:
  | S_NAME LPAREN LITERAL RPAREN    { ($1,$3) }
;

SimNamedLiteralL:
  | SimNamedLiteral                 { [$1] }
  | SimNamedLiteral 
    COMMA SimNamedLiteralL          { $1::$3 }
;  

SimObjLiteral:
  | SimNamedLiteralL                { $1 }
; 

SimLiteralL:
    LITERAL                         { [$1] }
  | LITERAL COMMA SimLiteralL       { $1::$3 }
;

SimVectorLiteral:
  | LCURLY SimLiteralL RCURLY       { $2 }
;  

  
SimVectorLiteralL:
    SimVectorLiteral                { [$1] }
  | SimVectorLiteral COMMA
    SimVectorLiteralL               { $1::$3 }
;

SimMatrixLiteral:
  | LCURLY SimVectorLiteralL RCURLY { $2 }
;  

SimLiteral:
  | SimVectorLiteral                { SVector $1 }
  | SimMatrixLiteral                { SMatrix $1 }
  | SimObjLiteral                   { SObject $1 }
  | LITERAL                         { SLiteral $1 }
  | TRUE                            { SLiteral (t_bool,"true" ,None)}
  | FALSE                           { SLiteral (t_bool,"false",None)}
;

SimLiteralOpt:
    /* empty */                     { SNull }
  | SimLiteral                      { $1 }
;

SimActualArg:
    LPAREN SimLiteralOpt RPAREN     { $2 }
;

/* -------------------------------------------------------------------------- */
SimDot:
    SimDotL                         { String.concat "." (List.rev $1) }
;

SimDotL:
    S_NAME                          { [$1] }
  | SimDotL DOT S_NAME              { $3::$1 }
;

/* -------------------------------------------------------------------------- */
Sim2NL:
    EOF                             { CmdUnit }
  | S_EOL                           { CmdUnit } 
  | S_COMMENT S_EOL                 { CmdUnit }
  | S_COMMENT EOF                   { CmdUnit }
;
  
/* -------------------------------------------------------------------------- */
SimFieldDef:
    S_EXP SimClassName SimArrayDimOpt S_NAME LITERAL Sim2NL
                                    {{spec={idname=$4;idtype=$2;iddim=$3 };
                                      rep=(SInPlace (lit2int32 $5))}
                                    }
  | S_REF SimClassName SimArrayDimOpt S_NAME LITERAL Sim2NL
                                    { {spec={idname=$4;idtype=$2;iddim=$3 };
                                       rep=(SByReference (lit2int32 $5))}
                                    }
;

SimFieldDefL:
    /* empty */                     { [] }     /* no field definition */
  | SimFieldDefL SimFieldDef        { $2::$1 }
;

SimFields:
    SimFieldDefL                   { List.rev $1 }

SimTagDef:
   | S_MTAG LPAREN LITERAL COMMA LITERAL RPAREN Sim2NL
                                    { SMTag (lit2int $3,lit2int $5) }
   | S_GTAG LPAREN LITERAL COMMA LITERAL RPAREN Sim2NL
                                    { SGTag (lit2int $3,lit2int $5) }
;

SimConstructor:
   | /* empty */                    { None }
   | S_CONSTR LITERAL Sim2NL        { Some (lit2int $2) }
;

SimTagDefL:
    /* empty */                     { [] }     /* no field definition */
  | SimTagDefL SimTagDef            { $2::$1 }
;

SimTags:
    SimTagDefL                      { List.rev $1 }
;

SimArrayDimL:
  | LITERAL                         { [lit2int $1] }
  | LITERAL COMMA LITERAL           { [lit2int $1;lit2int $3] }
;

SimArrayDimOpt:
    /* empty */                     { [] }
  | LSQURE SimArrayDimL RSQURE      { $2 }
;

SimSigTypeOpt:
    /* empty */                     { {idname="value";idtype="void";iddim=[] } }
  | LT S_NAME SimArrayDimOpt GT      
                                    { {idname="value";idtype=$2;iddim=$3 } }
;

SimSignalDef:
    S_IN  SimSigTypeOpt
                   SimDot    Sim2NL {{signame=$3; sigflow=SInput ; sigvalt=$2}}
  | S_OUT SimSigTypeOpt
                   SimDot    Sim2NL {{signame=$3; sigflow=SOutput; sigvalt=$2}}
  | S_LCL SimSigTypeOpt
                   SimDot    Sim2NL {{signame=$3; sigflow=SLocal ; sigvalt=$2}}
;

SimSignalDefL:
    /* empty */                     { [] }     /* no signal definition */
  | SimSignalDefL SimSignalDef      { $2::$1 }
;

SimSignals:
    SimSignalDefL                   { $1 }


SimTextSelection:
  | LPAREN RPAREN                   { {tselfilex =((-1));
                                       tselgo    =(  0 );
                                       originline=(  0 );
                                       origincol =(  0 );
                                       cornerline=(  0 );
                                       cornercol =(  0 ) }
                                    }
  | LPAREN SimTextLabel RPAREN      { $2 }
;

SimTextLabel:
  | LITERAL COMMA LITERAL COMMA LITERAL COMMA
    LITERAL COMMA LITERAL COMMA LITERAL
                                    { {tselfilex =lit2int $1;
                                       tselgo    =lit2int $3;
                                       originline=lit2int $5;
                                       origincol =lit2int $7;
                                       cornerline=lit2int $9;
                                       cornercol =lit2int $11}
                                    }
;

SimLabelDef:
   | LITERAL COLON LPAREN LITERAL COMMA LITERAL RPAREN Sim2NL
                                    { lit2int $1,
                                      LblGO( { gselfilex=lit2int $4;
                                               gselgo   =lit2int $6}) }
   | LITERAL COLON  SimTextSelection COLON LITERAL Sim2NL
                                    { lit2int $1,
                                       let knd = lit2int $5 in
                                       if knd = 1 then 
                                          LblHaltSel($3)
                                       else if knd = 2 then
                                          LblEmitSel($3)
                                       else 
                                          LblTextSel($3) }
 ;

SimLabelDefL:
    /* empty */                     { [] }     /* no label definition */
  | SimLabelDefL SimLabelDef        { $2::$1 }
;
 
SimLabels:
    SimLabelDefL                    { $1 }
;

SimTraceInfo:
    /* empty */                     { None }
  | S_SESIM LITERAL LITERAL Sim2NL { Some {sim_version=lit2string $2;
                                           nb_instants=lit2int $3} }
;

SimPrincipalSignals:
    /* empty */                     { [] }
  | S_PRINCIPALSIGNALS LCURLY Sim2NL
    SimSignals
    RCURLY Sim2NL                   { $4 }

SimTraceHeader:
    SimTraceInfo
    SimPrincipalSignals             { { traceinfo=$1; principal_signals=$2 } }
;

SimClass:
    S_CLASS SimClassName
    SimTextSelection LCURLY Sim2NL
    SimSignals
    SimFields
    SimConstructor
    SimTags
    SimLabels
    RCURLY Sim2NL                   { { classnm=$2;
                                        classtext=$3;
                                        classgraficfiles=[];
                                        signals=$6;
                                        members=$7;
                                        constr_label=$8;
                                        tags=$9;
                                        labels=$10} }
;

SimClassL:
    /* empty */                     { [] }
  | SimClassL SimClass              { $2::$1 }
;

SimClasses:
    SimClassL                       { List.rev $1 }
;

SimTiming:
    S_TIMING SimTime Sim2NL         { $2 } 
;

SimFileAssoc:
    LITERAL LITERAL Sim2NL          { (lit2int $1,lit2string $2) }
;

SimFileAssocL:
    /* empty */                     { [] }
  | SimFileAssocL SimFileAssoc      { $2::$1 }
;

SimFileAssociations:
    S_FILENAMES LCURLY Sim2NL
    SimFileAssocL
    RCURLY Sim2NL                   { List.rev $4 }
;

SimConfiguration:
    S_CONF LITERAL S_NAME Sim2NL    { {root_obj=lit2int32 $2;root_class=$3} }
;

SimTargetHeader:
    SimSignals
    SimClasses
    SimTiming
    SimFileAssociations
    SimConfiguration                { { target_signals  = $1;
                                        target_classes  = $2;
                                        target_timing   = $3;
                                        target_files    = $4;
                                        target_config   = $5
                                    } }
;

SimBreak:
    /* empty */                     { false } /* no break */
  | S_BREAK Sim2NL                  { true }
;

SimEmitIn:
    ADD SimDot SimActualArg Sim2NL { { emitname=$2;emitarg=$3;emitflow=None } }
;
SimEmitInL:
    /* empty */                     { [] }     /* no signal emit */
  | SimEmitInL SimEmitIn            { $2::$1 }
;
SimEmitInputs:
    SimEmitInL                      { List.rev $1 }
;

SimInstantPlus:
    SimBreak
    SimEmitInputs
    S_INSTANTPLUS SimOptTime Sim2NL { {ip_break=$1; ip_emit=$2; ip_time=$4} }

;

SimEmitOut:
    SUB SimDot SimActualArg Sim2NL { { emitname=$2;emitarg=$3;emitflow=None } }
;

ColonOpt:
    /* empty */                     { }
  | COLON                           { }   
;

Sim_Label:
   | LITERAL ColonOpt               { $1 }
;

Sim_LabelL:
      /* empty */                   { [] }
    | Sim_LabelL Sim_Label          { (lit2int $2)::$1 }
;

Sim_Labels:
    Sim_LabelL                      { List.rev $1 }

SimDbgOut:
    S_LBL SimDot COMMA Sim_Labels Sim2NL   { { sim_obj= $2;sim_lbl = $4} }
;

SimOutMsg:
  | SimEmitOut                      { Sim_Emit  $1 }
  | SimDbgOut                       { Sim_Dbg $1 }
;

SimOutMsgL:
  | /* empty */                     { [] }     /* no message */
  | SimOutMsgL SimOutMsg            { $2::$1  (* the sequence is reversed *) }
;

SimInstantMinus:
    SimOutMsgL S_EXCEPTION LPAREN LITERAL RPAREN SimOptTime Sim2NL
                                    { { im_outmsg = [Sim_Exc $4];
                                        im_time = $6 } }  
  | SimOutMsgL S_INSTANTMINUS SimOptTime Sim2NL
                                    { { im_outmsg = $1; im_time = $3 } }  
;

SimSig:
    SimDot SimActualArg             { { emitname=$1;emitarg=$2;emitflow=None } }
;


SimSigL:
  | /* empty */                     { [] }
  | SimSigL SimSig                  { $2::$1 }
;

SimInstant:
    EOF                              { { tr_break=false; tr_emit=[];
                                         tr_outmsg=[];tr_eof = true } } 
  | S_BREAK SEMI                     { { tr_break=true; tr_emit=[];
                                         tr_outmsg=[] ;tr_eof=false } }
  | SimSigL
    ARROW
    SimSigL
    SEMI                             { { tr_break=false; tr_emit=$1;
                                         tr_outmsg=$3  ;tr_eof = false } }
;

/* -------------------------------------------------------------------------- */
SimAbort:
    S_ABORT                         { $1 }
;
