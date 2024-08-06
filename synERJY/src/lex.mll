{
open Ast
open Sim_type
open Yacc
open Ly
open Util
open Util_parse

let keywords  = Hashtbl.create 127
let cmdwords  = Hashtbl.create 127
let simwords  = Hashtbl.create 127

let _ =
    List.iter (fun (ident, token) -> Hashtbl.add keywords ident token) [
(* syntax-highlight: sEerr *)
    "equal"                , GARBAGE ;
    "not_equal"            , GARBAGE ;
    "simple"               , GARBAGE ;
    "finally"              , GARBAGE ;
    "try"                  , GARBAGE ;
    "catch"                , GARBAGE ;
    "synchronized"         , GARBAGE ;

(* syntax-highlight: sEval *)
    "false"                , FALSE ;
    "true"                 , TRUE ;
    "null"                 , NULLOBJ ;

(* syntax-highlight: sEtyp *)
    "bool"                 , BOOL ;
    "boolean"              , BOOL ;
    "byte"                 , BYTE ;
    "int8"                 , BYTE ;
    "char"                 , CHAR ;
    "uint8"                , CHAR ;
    "short"                , SHORT ;
    "int16"                , SHORT ;
    "ushort"               , UINT16 ;
    "uint16"               , UINT16 ;
    "int"                  , INT ;
    "int32"                , INT ;
    "uint32"               , UINT32 ;
    "uint"                 , UINT32 ;
    "long"                 , LONG ;
    "int64"                , LONG ;
    "uint64"               , UINT64 ;
    "ulong"                , UINT64 ;
    "float"                , FLOAT ;
    "double"               , DOUBLE ;
    "object"               , OBJECT ;
    "time"                 , TIME ;
    "unsigned"             , UNSIGNED ;
    "String"               , STRING ;
    "Null"                 , NULLTYPE ;
    "Signal"               , SIGNAL ;
    "Delayed"              , DELAYEDSIGNAL ;
    "DelayedSignal"        , DELAYEDSIGNAL ;
    "Sensor"               , SENSOR ;

(* syntax-highlight: sEkw *)
    "A"              , ALL ;
    "abstract"       , ABSTRACT ;
    "activate"       , ACTIVATE ;
    "active"         , ACTIVE ;
    "actl"           , ACTL ;
    "assert"         , ASSERT ;
    "automaton"      , AUTOMATON ;
    "await"          , AWAIT ;
    "constraint"     , CONSTRAINT ;
    "blackboard"     , BLACKBOARD ;
    "break"          , BREAK ;
    "cancel"         , CANCEL ;
    "case"           , CASE ;
    "class"          , CLASS ;
    "continue"       , CONTINUE ;
    "ctl"            , CTL ;
    "current"        , UP_SPL ;
    "default"        , DEFAULT ;
    "do"             , DO ;
    "dt"             , DELTAT ;
    "during"         , DURING ;
    "else"           , ELSE ;
    "emit"           , EMIT ;
    "entry"          , ENTRY ;
    "exit"           , EXIT ;
    "extends"        , EXTENDS ;
    "fairness"       , FAIR ;
    "final"          , FINAL ;
    "for"            , FOR ;
    "goto"           , GARBAGE ;
    "halt"           , HALT ;
    "if"             , IF ;
    "implements"     , IMPLEMENTS ;
    "import"         , IMPORT ;
    "init"           , INIT ;
    "instanceof"     , INSTANCEOF ;
    "instant"        , INSTANT ;
    "interface"      , INTERFACE ;
    "interrupt"      , INTERRUPT ;
    "invariant"      , INVARIANT ;
    "loop"           , LOOP ;
    "ltl"            , LTL ;
    "native"         , NATIVE_FROM_C ;
    "import_from_C"  , NATIVE_FROM_C ;
    "export_to_C"    , NATIVE_TO_C ;
    "new"            , NEW ;
    "next"           , NEXT ;
    "node"           , NODE ;
    "nothing"        , NOTHING ;
    "parameter"      , PARAMETER ;
    "private"        , PRIVATE ;
    "protected"      , PROTECTED ;
    "proposition"    , PROPOSITIONS ;
    "D"              , DIAGONAL ;
    "pre"            , PRE ;
    "post"           , POST ;
    "ptl"            , PTL ;
    "public"         , PUBLIC ;
    "return"         , RETURN ;
    "schedule"       , SCHEDULE ;
    "precedence"     , PRECEDENCE ;
    "reactive"       , REACTIVE ;
    "state"          , STATE ;
    "static"         , STATIC ;
    "strictfp"       , STRICTFP ;
    "strongly"       , STRONGLY ;
    "super"          , SUPER ;
    "sustain"        , SUSTAIN ;
    "switch"         , SWITCH ;
    "then"           , THEN ;
    "this"           , THIS ;
    "throw"          , THROW ;
    "transient"      , TRANSIENT ;
    "until"          , UNTIL ;
    "void"           , VOID ;
    "volatile"       , VOLATILE ;
    "when"           , WHEN ;
    "while"          , WHILE ;

(* syntax-highlight: sEop *)
    "A."              , MC_AU ;
    "AF:"             , MC_AF ;
    "AG:"             , MC_AG ;
    "AX:"             , MC_AX ;
    "E:"              , MC_EU ;
    "EF:"             , MC_EF ;
    "EG:"             , MC_EG ;
    "EX:"             , MC_EX ;
    "X:"              , MC_X ;
    "G:"              , MC_G ;
    "F:"              , MC_F ;
    "Y:"              , MC_Y ;
    "Z:"              , MC_Z ;
    "H:"              , MC_H ;
    "O:"              , MC_O ;
    ":U:"             , MC_U ;
    ":V:"             , MC_V ;
    ":S:"             , MC_S ;
    ":T:"             , MC_T ;
]

let _ =
    List.iter (fun (ident, token) -> Hashtbl.add cmdwords ident token) [
    "application"       , C_APPLICATION ;
    "binary"            , C_BINARY ;
    "blif"              , C_BLIF ;
    "browser"           , C_BROWSER ;
    "build"             , C_BUILD ;
    "C-code"            , C_C ;
    "cfile"             , C_CFILE ;
    "cclib"             , C_CCLIB ;
    "check"             , C_CHECK ;
    "class"             , C_CLASS ;
    "clear"             , C_CLEAR ;
    "code"              , C_CODE ;
    "configuration"     , C_CONF ;
    "database"          , C_DATABASE ;
    "debug"             , C_DEBUG ;
    "editor"            , C_EDITOR ;
    "execute"           , C_EXECUTE ;
    "file"              , C_FILE ;
    "font"              , C_FONT ;
    "formula"           , C_FORMULA ;
    "fpga-verilog"      , C_VERILOG ;
    "graphic"           , C_GRAPHIC ;
    "height"            , C_HEIGHT ;
    "hfile"             , C_HFILE ;
    "Host"              , C_HOST ;
    "hostos"            , C_HOSTOS ;
    "kind"              , C_KIND ;
    "level"             , C_LEVEL ;
    "load"              , C_LOAD ;
    "make"              , C_MK ;
    "makefile"          , C_MAKEFILE ;
    "microsecond"       , C_MICROSECOND ;
    "millisecond"       , C_MILLISECOND ;
    "mc"                , C_MC ;
    "model"             , C_MODEL ;
    "object"            , C_OBJECT ;
    "parallel"          , C_PARALLEL ;
    "path"              , C_PATH ;
    "port"              , C_PORT ;
    "prefix"            , C_PREFIX ;
    "print"             , C_PRINT ;
    "project"           , C_PROJECT ;
    "quit"              , C_QUIT ;
    "save"              , C_SAVE ;
    "sefile"            , C_SEFILE ;
    "secfile"           , C_SECFILE ;
    "set"               , C_SET ;
    "signal"            , C_SIGNAL ;
    "show"              , C_SHOW ;
    "system"            , C_SYSTEM ;
    "simulator"         , C_SIMTOR ;
    "Simulation"        , C_SIM;
    "Simulink"          , C_SIMULINK ;
    "Scicos"            , C_SCICOS ;
    "size"              , C_SIZE ;
    "style"             , C_STYLE ;
    "target"            , C_TARGET ;
    "timescale"         , C_TIMESCALE ;
    "test"              , C_TEST ;
    "trace"             , C_TRACE ;
    "uploadbutton"      , C_UPLOADBUTTON ;
    "Verification"      , C_VERIFICATION ;
    "Verilog"           , C_VERILOG ;
    "VerilogSimulation" , C_VERILOGSIM ;
    "vis-model"         , C_VISMODEL ;
    "weight"            , C_WEIGHT ;
    "width"             , C_WIDTH ;
    "window"            , C_WINDOW ;
    "workspace"         , C_WORKSPACE ;
    "Matlab"            , C_MATLAB ;
    "Scilab"            , C_SCILAB ;
    "NuSMV"             , C_NUSMV ;
    "directory"         , C_DIRCTRY ;
]

let _ =
    List.iter (fun (ident, token) -> Hashtbl.add simwords ident token) [
    "sEsim"            , S_SESIM ;
    "$signals"         , S_PRINCIPALSIGNALS;
    "$filenames"       , S_FILENAMES;
    "class"            , S_CLASS;
    "$exp"             , S_EXP ;
    "$ref"             , S_REF ;
    "$constr"          , S_CONSTR ;
    "$mtag"            , S_MTAG ;
    "$gtag"            , S_GTAG ;
    "timing"           , S_TIMING ;
    "$configuration"   , S_CONF ;
    "$sensor"          , S_IN ;
    "$signal"          , S_OUT ;
    "$local"           , S_LCL ;
    "$val"             , S_VAL ;
    "$get"             , S_GET ;
    "true"             , TRUE ;
    "false"            , FALSE ;
]

let mk_lit_typ i =
    let lt i bit =
        let c = Int64.shift_left Int64.one bit in
            Int64.compare i c < 0
    in
    if Int64.compare i (Int64.zero) >= 0 then
       if lt i  7 then t_cuint7  else
       if lt i  8 then t_cuint8  else
       if lt i 15 then t_cuint15 else
       if lt i 16 then t_cuint16 else
       if lt i 31 then t_cuint31 else
       if lt i 32 then t_cuint32 else
                       t_cuint63 (* cuint64 not really needed :-) *)
    else
       let i = Int64.neg i in
           if lt i  7 then t_cint8  else
           if lt i 15 then t_cint16 else
           if lt i 31 then t_cint32 else
                           t_cint64

let check_time_long_short_byte s l =
    let chk  len str     = (l > len) && (String.sub s (l-len) len) = str in
    let d2i c = if c >= 97 && c <= 102 then c - 87 else
                if c >= 65 && c <= 70  then c - 55 else
                if c >= 48 && c <= 57  then c - 48 else
                   raise Not_found
    in
        if l = 3 && String.sub s 0 1 = "'" && String.sub s (l-1) 1 = "'" then (
           LITERAL(t_cuint8,s,Some (Int64.of_int (Char.code (String.get s 1))))
        ) else if l > 3 && (let s = String.sub s 0 2  in s="0x" || s="0X") then (
           let c = ref Int64.zero in
               for i = 2 to (l-1) do
                   let d = Int64.of_int (d2i (Char.code (String.get s i))) in
                       c := Int64.add (Int64.shift_left !c 4) d
               done;
               LITERAL(mk_lit_typ !c,s,Some !c)
        (* do not change the sequence below: sec is substring of [mu]sec ! *)
        ) else if chk 4 "hour" then (
           let v1 = Int64.of_string (String.sub s 0 (l-4))
           and v2 = Int64.of_int 3600 and v3 = Int64.of_int (1000000) in
               LITERAL(t_time,s,Some (Int64.mul v1 (Int64.mul v2 v3)))
        ) else if chk 4 "msec" then (
           let v1 = Int64.of_string (String.sub s 0 (l-4))
           and v2 = Int64.of_int (1000) in
               LITERAL(t_time,s,Some (Int64.mul v1 v2))
        ) else if chk 4 "usec" then (
           let v1 = Int64.of_string (String.sub s 0 (l-4)) in
               LITERAL(t_time,s,Some v1)
        ) else if chk 3 "min" then (
           let v1 = Int64.of_string (String.sub s 0 (l-3))
           and v2 = Int64.of_int (60000000) in
               LITERAL(t_time,s,Some (Int64.mul v1 v2))
        ) else if chk 3 "sec" then (
           let v1 = Int64.of_string (String.sub s 0 (l-3))
           and v2 = Int64.of_int (1000000) in
               LITERAL(t_time,s,Some (Int64.mul v1 v2))
        ) else
           let i = Int64.of_string s in
               LITERAL(mk_lit_typ i,s,Some i)

let mk_negnumeric s =
    let s = String.copy s in
        try
           let i = Int64.of_string s in
               LITERAL(mk_lit_typ i,s,Some i)
        with _ ->
           raise (Parse("invalid numeric constant: "^s))

let dotted_sec s =
    let s = String.copy s in
    try
       let len = String.length s     in
       let dot = String.index  s '.' in
           let rlen = let l = len - dot - 4 in if l>6 then 6 else l in
           let sec  = String.sub s 0 dot in
           let sec  = if sec  = "" then Int64.zero else Int64.of_string sec in
           let usec = String.sub s (dot+1) rlen in
           let usec = let scale = [|0;100000;10000;1000;100;10;1|].(rlen) in
                          if usec = ""
                             then Int64.zero
                             else let scale = Int64.of_int scale in
                                      Int64.mul (Int64.of_string usec) scale in
           let mio = Int64.of_int (1000000) in
           let tm  = Int64.add (Int64.mul sec mio) usec in
               LITERAL(t_time,s,Some tm)
    with _ ->
       raise (Parse ("time constant "^(String.copy s)^" is invalid"))

let cmd_keyword ls =
    try
        Hashtbl.find cmdwords ls
    with Not_found ->
        raise (Parse ("invalid command: "^(String.copy ls)))

let cmd_token ls =
    let s = String.copy ls in
    let l = (String.length s) - 1 in
    let c = String.get s 0 in
    let f = Char.code c in
    let all_digits = ref true
    in
        if (c = '"') && ((String.get s l) = '"') then
           (CmdString (String.sub s 1 (l-1)))
        else if (digit f) then
           if l = 0
              then (CmdInt (s2i s))
              else ( for i = 1 to l do
                         if not( digit (Char.code (String.get s i)) )
                            then all_digits := false
                     done;
                     if (!all_digits)
                        then (CmdInt (s2i s))
                        else (CmdString s)
                   )
        else (CmdString s)

let sim_token ls =
    try
        Hashtbl.find simwords ls
    with Not_found ->
    let s  = String.copy ls in
    let l  = (String.length s) in
    let f  = Char.code (String.get s 0 ) in
        if lower f || upper f || f = 95
           then ( S_NAME s )
           else ( try
                    check_time_long_short_byte s l
                  with _ ->
                    raise (Sim_error.SimError
                       ("invalid numeric or time constant: "^s))
                )

let chk_name_legal s =
    let len   = String.length s in
    let only_upper = ref true
    and underscore = ref false in
    let check c =
        if (c = 95) (* 95 = Char.code '_' *)
           then if !underscore
                then raise (Parse "name containing '__' is invalid")
                else underscore := true
           else underscore := false;
        if (lower c) then (
           only_upper := false;
        )
    in
        for i = 0 to len-1 do
            check (Char.code (String.get s i))
        done;
        !only_upper

let mk_class_or_upperid s =
    try
        Hashtbl.find keywords s
    with Not_found ->
        if Char.code (String.get s ((String.length s)-1)) = 58 (* ':' *)
           then raise (Parse("Invalid identifier. Looks like a label, but a \
                              label has to start with a lower case character \
                              and to terminate with two colons. Found: "^s))
           else ();
        let s = String.copy s             in
        let only_upper = chk_name_legal s in
            if only_upper && String.length s > 1
               then AN_ID (mf2id s)
               else A_CLASS (c2id  s)

let mk_id s =
    try
        Hashtbl.find keywords s
    with Not_found ->
        let s = String.copy s    in
        let _ = chk_name_legal s in
            AN_ID (mf2id s)

let mk_buffer s =
    let len  = String.length s in
    let s = String.sub s 0 (len-2) in
    try
        Hashtbl.find keywords s
    with Not_found ->
        let s = String.copy s    in
        let _ = chk_name_legal s in
            A_BUFFER (mf2id s)

let mk_derived s =
    let len  = String.length s in
    let s = String.sub s 0 (len-1) in
    try
        Hashtbl.find keywords s
    with Not_found ->
        let s = String.copy s    in
        let _ = chk_name_legal s in
            A_DERIVED (mf2id s)

let mk_label s =
    let len  = String.length s in
    let s = String.sub s 0 (len-2) in
    let _   = chk_name_legal s in
        LABEL (mf2lbl (mf2id s))

let mk_const s =
    let s = String.copy s     in
    let l = (String.length s) in
        try
           check_time_long_short_byte s l
        with _ ->
           raise (Parse("invalid numeric or time constant: "^s))

let mk_string s =
    let l = String.length s in
    let s = if l <= 2
               then ""
               else String.sub s 1 (l-2) in
        LITERAL(t_string,s,None)

let mk_floatformat s =
    let err () = raise (Parse("invalid float or double constant")) in
    let l = String.length s - 1 in
    let c = String.get s l      in
    let (s,t) = if c = 'f' || c = 'F' then (
                   (String.sub s 0 l,t_float)
                ) else if digit (Char.code c) then (
                   (String.copy s,t_double)
                ) else if c = 'd' || c = 'D' then (
                   (String.sub s 0 l,t_double)
                ) else (
                   err ()
                )
    in
        try
          let _ = float_of_string s in
              LITERAL(t,s,None)
        with _ ->
          err ()

}

let alfanum = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let whitesp = [' ' '\t' '\r']
let digit   = ['0'-'9']

(* ---------------------------------------------------------- simulator --- *)
rule prs_sim_toeol = parse

  eof           { "" }
| [^'\n']*      { Lexing.lexeme lexbuf }

and prs_sim is_trace = parse

  eof      { EOF }
| '+'      { ADD }
| '-'      { SUB }
| '='      { EQ }
| '<'      { LT }
| '>'      { GT }
| '{'      { LCURLY }
| '}'      { RCURLY }
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LSQURE }
| ']'      { RSQURE }
| ':'      { COLON }
| ','      { COMMA }
| ';'      { SEMI }
| "[]"     { ARRAY1 }
| "[,]"    { ARRAY2 }
| '.'      { DOT }
| "at:"    { S_LBL }
| "--i"    { S_INSTANTMINUS }
| "--exc"  { S_EXCEPTION }
| "++i"    { S_INSTANTPLUS }
| "++t"    { S_INSTANTPLUS }
| "->"     { ARROW }

| whitesp* '\n'                      { Sim_type.sim.parsebuf.pb_line <-
                                           Sim_type.sim.parsebuf.pb_line + 1;
                                       if is_trace
                                       then prs_sim is_trace lexbuf
                                       else S_EOL
                                     }

| "//"[^'\n']*"\n"                   { Sim_type.sim.parsebuf.pb_line <-
                                           Sim_type.sim.parsebuf.pb_line + 1;
                                       prs_sim is_trace lexbuf
                                     }
                                        
| "#"[' ' '\t']*"break"              { S_BREAK }
| "$abort"[' ' '\t']*                { S_ABORT(prs_sim_toeol lexbuf) }

| ('$')? alfanum+                    { sim_token (Lexing.lexeme lexbuf) }

| whitesp+                           { prs_sim is_trace lexbuf }

| '\''[^'\'']*'\'' { mk_const (Lexing.lexeme lexbuf) }
| '"'[^'"']*'"'    { mk_string (Lexing.lexeme lexbuf) }

| '-' digit+       { mk_negnumeric (Lexing.lexeme lexbuf) }

| ['-']? digit+ '.' digit+ (['E' 'e']['+' '-']?digit+)?['d' 'D' 'f' 'F']?
                   { mk_floatformat (Lexing.lexeme lexbuf) }
| digit+ '.' digit* "sec"
                   { dotted_sec (Lexing.lexeme lexbuf) }

(* ------------------------------------------------------------ command --- *)
and prs_cmd = parse

  eof                                { EOF }
| '='  whitesp*                      { C_EQ (prs_cmd_eq lexbuf) }
| "+=" whitesp*                      { C_CONC (prs_cmd_eq lexbuf) }
| "//" [^'\n']*                      { prs_cmd lexbuf }
| '\n'                               { pos_1line lexbuf; prs_cmd lexbuf }
| whitesp+                           { prs_cmd lexbuf }
| [^' ' '\t' '\n' '=' ';']+          { cmd_keyword (Lexing.lexeme lexbuf) }
| ';'                                { SEMI }

and prs_cmd_eq = parse
  [^'"' ';'] [^';']+                 { cmd_token (Lexing.lexeme lexbuf) }
| ['"'] [^'"']+ ['"']                { cmd_token (Lexing.lexeme lexbuf) }
| ['0'-'9']                          { cmd_token (Lexing.lexeme lexbuf) }

and prs_comment = parse
  eof           { EOF }
| "*/"          { prs_tk lexbuf }
| '\n'          { pos_1line lexbuf; prs_comment lexbuf }
| [^'\n' '*']*  { prs_comment lexbuf }
| '*'           { prs_comment lexbuf }

and prs_toeol = parse
  eof           { EOF }
| '\n'          { pos_1line lexbuf; prs_tk lexbuf }
| [^'\n']*      { prs_toeol lexbuf }


(* ----------------------------------------------------------------- sE --- *)
and prs_tk = parse

  eof      { EOF }

| "="      { ASSIGN(id_op_assign) }
| "*="     { ASSIGN(id_op_mult) }
| "/="     { ASSIGN(id_op_div) }
| "%="     { ASSIGN(id_op_mod) }
| "+="     { ASSIGN(id_op_add) }
| "-="     { ASSIGN(id_op_sub) }
| "<<="    { ASSIGN(id_op_leftshift) }
| ">>="    { ASSIGN(id_op_rightshift) }
| ">>>="   { ASSIGN(id_op_rightshift0) }
| "&="     { ASSIGN(id_op_bit_and) }
| "|="     { ASSIGN(id_op_bit_or) }
| "^="     { ASSIGN(id_op_xor) }

| '?'      { QUESTION }
| '$'      { DOLLAR }
| '@'      { AT }
| ":="     { FLOWEQU }

| ','      { COMMA }
| ';'      { SEMI }
| ':'      { COLON }
| ".."     { DOTDOT }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LCURLY }
| '}'      { RCURLY }
| "{|"     { LDFLOW }
| "|}"     { RDFLOW }
| "[]"     { ARRAY1 }
| "[,]"    { ARRAY2 }
| '['      { LSQURE }
| ']'      { RSQURE }
| "[["     { PAR_BEGIN }
| "]]"     { PAR_END }
| "||"     { BAR2 }

| '.'      { DOT }

| "=="     { EQ }
| '!'      { NOT }
| "&&"     { ANDCOND }
| '&'      { AND }
| '|'      { OR }
| "xor"    { XOR }
| '^'      { XOR }
| "->"     { ARROW }
| "=>"     { STATEARROW }
| "<->"    { EQUIV }
| "++"     { INCR }
| "--"     { DECR }
| '+'      { ADD }
| '-'      { SUB }
| '*'      { MULT }
| '/'      { DIV }
| '%'      { MOD }
| "<="     { LE }
| ">="     { GE }
| "!="     { NE }
| '<'      { LT }
| '>'      { GT }
| '~'      { BITCOMPLEMENT }

| "^t"     { TRANSP }
| ".*"     { POINTMULT }

| ")" whitesp* "[" { RPARENLSQURE }

| '\n'     { pos_1line lexbuf; prs_tk lexbuf }

| "(boolean)" { SIMPLE_IN_PAREN id_bool }
| "(bool)"   { SIMPLE_IN_PAREN id_bool }
| "(char)"   { SIMPLE_IN_PAREN id_char }
| "(byte)"   { SIMPLE_IN_PAREN id_byte }
| "(int8)"   { SIMPLE_IN_PAREN id_short }
| "(uint8)"  { SIMPLE_IN_PAREN id_uint16 }
| "(short)"  { SIMPLE_IN_PAREN id_short }
| "(int16)"  { SIMPLE_IN_PAREN id_short }
| "(uint16)" { SIMPLE_IN_PAREN id_uint16 }
| "(int)"    { SIMPLE_IN_PAREN id_int }
| "(int32)"  { SIMPLE_IN_PAREN id_int }
| "(uint32)" { SIMPLE_IN_PAREN id_uint32 }
| "(long)"   { SIMPLE_IN_PAREN id_long }
| "(int64)"  { SIMPLE_IN_PAREN id_long }
| "(uint64)" { SIMPLE_IN_PAREN id_uint64 }
| "(float)"  { SIMPLE_IN_PAREN id_float }
| "(double)" { SIMPLE_IN_PAREN id_double }
| "(String)" { SIMPLE_IN_PAREN id_string }

| "(boolean[])" { SIMPLE_ARRAY1_IN_PAREN t_bool }
| "(bool[])" { SIMPLE_ARRAY1_IN_PAREN t_bool }
| "(char[])" { SIMPLE_ARRAY1_IN_PAREN t_char }
| "(byte[])" { SIMPLE_ARRAY1_IN_PAREN t_byte }
| "(int8[])" { SIMPLE_ARRAY1_IN_PAREN t_short }
| "(uint8[])" { SIMPLE_ARRAY1_IN_PAREN t_uint16 }
| "(short[])" { SIMPLE_ARRAY1_IN_PAREN t_short }
| "(int16[])" { SIMPLE_ARRAY1_IN_PAREN t_short }
| "(uint16[])" { SIMPLE_ARRAY1_IN_PAREN t_uint16 }
| "(int[])" { SIMPLE_ARRAY1_IN_PAREN t_int }
| "(int32[])" { SIMPLE_ARRAY1_IN_PAREN t_int }
| "(uint32[])" { SIMPLE_ARRAY1_IN_PAREN t_uint32 }
| "(long[])" { SIMPLE_ARRAY1_IN_PAREN t_long }
| "(int64[])" { SIMPLE_ARRAY1_IN_PAREN t_long }
| "(uint64[])" { SIMPLE_ARRAY1_IN_PAREN t_uint64 }
| "(float[])" { SIMPLE_ARRAY1_IN_PAREN t_float }
| "(double[])" { SIMPLE_ARRAY1_IN_PAREN t_double }
| "(String[])" { SIMPLE_ARRAY1_IN_PAREN t_string }

| "(boolean[,])" { SIMPLE_ARRAY2_IN_PAREN t_bool }
| "(bool[,])" { SIMPLE_ARRAY2_IN_PAREN t_bool }
| "(char[,])" { SIMPLE_ARRAY2_IN_PAREN t_char }
| "(byte[,])" { SIMPLE_ARRAY2_IN_PAREN t_byte }
| "(int8[,])" { SIMPLE_ARRAY2_IN_PAREN t_short }
| "(uint8[,])" { SIMPLE_ARRAY2_IN_PAREN t_uint16 }
| "(short[,])" { SIMPLE_ARRAY2_IN_PAREN t_short }
| "(int16[,])" { SIMPLE_ARRAY2_IN_PAREN t_short }
| "(uint16[,])" { SIMPLE_ARRAY2_IN_PAREN t_uint16 }
| "(int[,])" { SIMPLE_ARRAY2_IN_PAREN t_int }
| "(int32[,])" { SIMPLE_ARRAY2_IN_PAREN t_int }
| "(uint32[,])" { SIMPLE_ARRAY2_IN_PAREN t_uint32 }
| "(long[,])" { SIMPLE_ARRAY2_IN_PAREN t_long }
| "(int64[,])" { SIMPLE_ARRAY2_IN_PAREN t_long }
| "(uint64[,])" { SIMPLE_ARRAY2_IN_PAREN t_uint64 }
| "(float[,])" { SIMPLE_ARRAY2_IN_PAREN t_float }
| "(double[,])" { SIMPLE_ARRAY2_IN_PAREN t_double }
| "(String[,])" { SIMPLE_ARRAY2_IN_PAREN t_string }

| ['a'-'z' '_'] alfanum* "'"  { mk_derived (Lexing.lexeme lexbuf) }
| ['a'-'z' '_'] alfanum* ".." { mk_buffer (Lexing.lexeme lexbuf) }
| ['a'-'z'] alfanum* "::"     { mk_label (Lexing.lexeme lexbuf) }
| ['A'-'Z']     alfanum* ':'? { mk_class_or_upperid (Lexing.lexeme lexbuf) }
| ['a'-'z' '_'] alfanum*      { mk_id (Lexing.lexeme lexbuf) }
| digit alfanum*   { mk_const (Lexing.lexeme lexbuf) }
| whitesp+         { prs_tk lexbuf }

| "/*"             { prs_comment lexbuf }
| "//"             { prs_toeol lexbuf }
| '\''[^'\'']*'\'' { mk_const (Lexing.lexeme lexbuf) }
| '"'[^'"']*'"'    { mk_string (Lexing.lexeme lexbuf) }

| '-' digit+       { mk_negnumeric (Lexing.lexeme lexbuf) }

| ['-']?digit+'.'digit+(['E' 'e']['+' '-']?digit+)?['d' 'D' 'f' 'F']?
                   { mk_floatformat (Lexing.lexeme lexbuf) }
| digit+ '.' digit* "sec"
                   { dotted_sec (Lexing.lexeme lexbuf) }
