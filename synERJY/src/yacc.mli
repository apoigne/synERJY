type token =
  | C_EQ of (Ast.tcmd_int_or_string)
  | C_CONC of (Ast.tcmd_int_or_string)
  | C_APPLICATION
  | C_BINARY
  | C_BLIF
  | C_BROWSER
  | C_BUILD
  | C_C
  | C_CODE
  | C_CCLIB
  | C_CFILE
  | C_CHECK
  | C_CLASS
  | C_CLEAR
  | C_CONF
  | C_DATABASE
  | C_DEBUG
  | C_EDITOR
  | C_EXECUTE
  | C_FILE
  | C_FONT
  | C_FORMULA
  | C_VERILOG
  | C_GRAPHIC
  | C_HEIGHT
  | C_HFILE
  | C_HOST
  | C_HOSTOS
  | C_KIND
  | C_LEVEL
  | C_LOAD
  | C_MAKEFILE
  | C_MICROSECOND
  | C_MILLISECOND
  | C_MC
  | C_MK
  | C_MODEL
  | C_OBJECT
  | C_PARALLEL
  | C_PATH
  | C_PORT
  | C_PREFIX
  | C_PRINT
  | C_PROJECT
  | C_QUIT
  | C_SAVE
  | C_SET
  | C_SHOW
  | C_SEFILE
  | C_SECFILE
  | C_SIGNAL
  | C_SIM
  | C_SIMTOR
  | C_SIMULINK
  | C_SCICOS
  | C_SIZE
  | C_SYSTEM
  | C_TARGET
  | C_TEST
  | C_TIMESCALE
  | C_TRACE
  | C_UPLOADBUTTON
  | C_VERIFICATION
  | C_VERILOGSIM
  | C_VISMODEL
  | C_WEIGHT
  | C_WIDTH
  | C_WINDOW
  | C_WORKSPACE
  | C_MATLAB
  | C_NUSMV
  | C_SCILAB
  | C_STYLE
  | C_DIRCTRY
  | S_BREAK
  | S_CLASS
  | S_CONF
  | S_CONSTR
  | S_EOL
  | S_EXP
  | S_FILENAMES
  | S_GET
  | S_GTAG
  | S_IN
  | S_INSTANTMINUS
  | S_EXCEPTION
  | S_INSTANTPLUS
  | S_LBL
  | S_LCL
  | S_MTAG
  | S_OUT
  | S_PRINCIPALSIGNALS
  | S_REF
  | S_SESIM
  | S_TIMING
  | S_VAL
  | S_ABORT of (string)
  | S_COMMENT of (string)
  | S_NAME of (string)
  | EOF
  | GARBAGE
  | FALSE
  | TRUE
  | VOID
  | RPARENLSQURE
  | BOOL
  | BYTE
  | CHAR
  | SHORT
  | UINT16
  | INT
  | UINT32
  | LONG
  | UINT64
  | DOUBLE
  | FLOAT
  | OBJECT
  | STRING
  | TIME
  | UNSIGNED
  | NULLTYPE
  | SENSOR
  | SIGNAL
  | DELAYEDSIGNAL
  | ABSTRACT
  | ACTIVATE
  | ACTIVE
  | ACTL
  | ALL
  | ASSERT
  | AUTOMATON
  | AWAIT
  | CONSTRAINT
  | BLACKBOARD
  | BREAK
  | CANCEL
  | CASE
  | CLASS
  | CONTINUE
  | CTL
  | DEFAULT
  | DELTAT
  | DIAGONAL
  | DO
  | DURING
  | ELSE
  | EMIT
  | ENTRY
  | EXIT
  | EXTENDS
  | FAIR
  | FINAL
  | FOR
  | HALT
  | IF
  | IMPLEMENTS
  | IMPORT
  | INIT
  | INSTANCEOF
  | INSTANT
  | INTERFACE
  | INTERRUPT
  | INVARIANT
  | LOOP
  | LTL
  | NATIVE_FROM_C
  | NATIVE_TO_C
  | NEW
  | NEXT
  | NODE
  | NOTHING
  | NULLOBJ
  | PARAMETER
  | POST
  | PRE
  | PRIVATE
  | PROTECTED
  | PROPOSITIONS
  | PTL
  | PUBLIC
  | REACTIVE
  | RETURN
  | SCHEDULE
  | PRECEDENCE
  | STATE
  | STATIC
  | STRICTFP
  | STRONGLY
  | SUPER
  | SUSTAIN
  | SWITCH
  | THEN
  | THIS
  | THROW
  | TRANSIENT
  | UNTIL
  | UP_SPL
  | VOLATILE
  | WHEN
  | WHILE
  | MC_AF
  | MC_AG
  | MC_AU
  | MC_AX
  | MC_EF
  | MC_EG
  | MC_EU
  | MC_EX
  | MC_X
  | MC_G
  | MC_F
  | MC_Y
  | MC_Z
  | MC_H
  | MC_O
  | MC_U
  | MC_V
  | MC_S
  | MC_T
  | MC_HASBEEN
  | MC_ONCE
  | MC_PREVIOUS
  | MC_SINCE
  | QUESTION
  | AT
  | DOLLAR
  | ARRAY1
  | ARRAY2
  | COLON
  | COMMA
  | DOTDOT
  | TRANSP
  | LCURLY
  | LDFLOW
  | LPAREN
  | LSQURE
  | PAR_BEGIN
  | PAR_END
  | RCURLY
  | RDFLOW
  | RPAREN
  | RSQURE
  | SEMI
  | DOT
  | ADD
  | AND
  | ANDCOND
  | ASSIGN of (Ly.tmfid)
  | BAR2
  | BITCOMPLEMENT
  | DECR
  | DIV
  | EQ
  | EQUIV
  | FLOWEQU
  | GE
  | GT
  | ARROW
  | STATEARROW
  | INCR
  | LE
  | LT
  | MULT
  | NE
  | NOT
  | OR
  | MOD
  | POINTMULT
  | SUB
  | XOR
  | AN_ID of (Ly.tmfid)
  | A_BUFFER of (Ly.tmfid)
  | A_DERIVED of (Ly.tmfid)
  | LABEL of (Ly.tlbl)
  | A_CLASS of (Ly.tclass)
  | LITERAL of (Ast.ttype * string * int64 option)
  | SIMPLE_IN_PAREN of (Ly.tclass)
  | SIMPLE_ARRAY1_IN_PAREN of (Ast.ttype)
  | SIMPLE_ARRAY2_IN_PAREN of (Ast.ttype)

val prs_id :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ly.tmfid
val prs_classid :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ly.tclass
val prs_condtrans :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ttrans
val prs_inittrans :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.tstmtl
val prs_trans :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ttrans
val prs_cond :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.tthen_part
val prs_stateaction :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Util_parse.tstatemodif
val prs_import :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
val prs_class :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ly.tlbl
val prs_cmd :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.tcmd_to_do
val prs_sim_traceheader :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.traceheader_t
val prs_sim_class :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.simclass_t
val prs_sim_signals :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.signalspec_t list
val prs_sim_psignals :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.signalspec_t list
val prs_sim_classes :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.simclass_t list
val prs_sim_timing :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int64
val prs_sim_targetheader :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.targetheader_t
val prs_sim_instantplus :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.instantplus_t
val prs_sim_instantminus :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.instantminus_t
val prs_sim_instant :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.instant_t
val prs_sim_abort :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val prs_sim_valopt :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.simval_t
val prs_sim_outmsg :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sim_type.simoutmsg_t
