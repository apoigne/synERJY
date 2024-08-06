exception Error  of string
exception Parse  of string

(* -------------------------------------------------------------------------- *)
let str2id_tbl = Hashtbl.create 1991
let id2str_tbl = Hashtbl.create 1991

type tclass = int
type tmfid  = int
type tlbl   = int
type tsc_id = int
(* --------------------------------------------------------------------------
  persistent data for string management. All class, methods/fields,
  signal and type-variable identifier are replaced by ints of distinct
  ranges. The two hashtable store the mapping in both directions.

  the returned key for a string depends on its syntactic type
  -------------------------------------------------------------------------- *)
type tsrc_pos =
     | NoSrcPos
     | TextPos    of string * int * int*int        *  int*int
                  (* file, internal id of GO,  from-line,-col ,  to-line,-col *)
     | HaltPos    of string * int * int*int        *  int*int
                  (* file, internal id of GO,  from-line,-col ,  to-line,-col *)
     | EmitPos    of string * int * int*int        *  int*int
                  (* file, internal id of GO,  from-line,-col ,  to-line,-col *)
     | GraphicPos of string * int
                  (* file-id, internal id of GO *)
     (* id of GO = 0: no graphic object *) 

module LblMap = Map.Make( struct
                            type t = int
                            let compare x y = Pervasives.compare x y
                          end
                        )
type tlbl2sp = tsrc_pos LblMap.t

type tstring_mngt_persistent_data = 
   { mutable nl_at    : int list; (* position of new lines in a file *)
     mutable nl_stack : int list list;

     mutable id_class : int;      (* this and following have printable names *)
     mutable id_mfid  : int;

     mutable id_lbl   : int;      (* INTERNAL name generator *)
     mutable lbl2srcp : tlbl2sp;

     mutable sgn2str     : tsc_id -> string; (* see comment below *)
     mutable sgn2valstr  : tsc_id -> string; (* see comment below *)
     mutable rctsgn2str  : tsc_id -> string;
   }

let ly =
    let dflt_fun x = raise (Error("[[InternalError]] set_sc2str: not used")) in
         { nl_at      = [];
           nl_stack   = [];
           id_class   = 5000000;
           id_mfid    = 6000000;
           id_lbl     = 0;
           lbl2srcp   = LblMap.empty;
           sgn2str    = dflt_fun;
           sgn2valstr = dflt_fun;
           rctsgn2str = dflt_fun;
         }

(* --------------------------------------------------------------------------
  name generators
  class, method/field, type variable, signal -> internal name [int]
  -------------------------------------------------------------------------- *)
let c2id id =
    try
      Hashtbl.find str2id_tbl id
    with Not_found ->
      let c = ly.id_class in
          if c >= 6000000 then (
             raise (Error("[[InternalError]] c2id: too many classes"))
          ) else (
             ly.id_class <- c+1;
             Hashtbl.add str2id_tbl id c;
             Hashtbl.add id2str_tbl c id;
             c
          )

let mf2id id =
    try
      Hashtbl.find str2id_tbl id
    with Not_found ->
      let mf = ly.id_mfid in
          if mf >= 7000000 then (
             raise (Error("[[InternalError]] mf2id: too many methods/fields"))
          ) else (
             ly.id_mfid <- mf+1;
             Hashtbl.add str2id_tbl id mf;
             Hashtbl.add id2str_tbl mf id;
             mf
          )

let mf2lbl   x = x
let lbl2mf   x = x
let c2mf     x = x
let mf2c     x = x
let c2int    x = x
let mf2int   x = x
let int2c    x = x
let int2mf   x = x

(* --------------------------------------------------------------------------
  internal name -> class, method/field
  -------------------------------------------------------------------------- *)
let c2str cid =
    try
      Hashtbl.find id2str_tbl cid
    with Not_found ->
       "??"^(string_of_int cid)^"??" (* error ... *)

let mf2str mfid =
    try
      Hashtbl.find id2str_tbl mfid
    with Not_found ->
       if mfid >= 7000000
          then "fp"^string_of_int (mfid-7000000) (* formal [see SymcPar] ... *)
          else "??"^(string_of_int mfid)^"??"    (* error ... *)

(* --------------------------------------------------------------------------
  the programmer may supply (explicit) labels for stmts, otherwise the parser
  generates (implicit) labels. These are used to identify source positions
  in text files or graphic files.
  -------------------------------------------------------------------------- *)
let mk_int_lbl () =
    let c = ly.id_lbl + 1 in
          if c >= 5000000 then (
             raise (Error("[[InternalError]] int_mk_lbl: too many label"))
          ) else (
             ly.id_lbl <- c;
             c
          )

let is_int_lbl c =
    (c >= 0) && (c < 5000000)

let ext_lbl lbl sp =
    if LblMap.mem lbl ly.lbl2srcp
       then raise (Error("[[InternalError]] ext_label:dup:"^mf2str lbl))
       else ly.lbl2srcp <- LblMap.add lbl sp ly.lbl2srcp

let int_lbl sp =
    let lbl = mk_int_lbl () in
        ly.lbl2srcp <- LblMap.add lbl sp ly.lbl2srcp;
        lbl

let lbl2str lbl =
    try
      Hashtbl.find id2str_tbl lbl
    with Not_found ->
      string_of_int lbl

let is_specified_label lbl =
    Hashtbl.mem id2str_tbl lbl

let int_lbl2str lbl =
    string_of_int lbl

let nolbl = 0

let lbl2srcp lbl lbl2srcp =
    try
       LblMap.find lbl lbl2srcp
    with Not_found ->
       if lbl = nolbl
          then NoSrcPos
          else raise (Error("[[InternalError]] lbl2srcp:invlabel"))

let lbl2srcp_iter lbl2srcp f =
    LblMap.iter f lbl2srcp

let lbl2sp_empty = LblMap.empty

let clr_and_return_lbls () =
    let lbl2srcp = ly.lbl2srcp in
        ly.lbl2srcp <- LblMap.empty;
        lbl2srcp

let lbl2srcp_text2emit lbl lbl2srcp =
    if lbl = nolbl then (
        lbl2srcp
    ) else (
        try  match LblMap.find lbl lbl2srcp with
             | TextPos(f,go,ul,uc,ll,lc)
                 -> LblMap.add lbl (EmitPos(f,go,ul,uc,ll,lc))
                             (LblMap.remove lbl lbl2srcp)
             | _ -> lbl2srcp
        with Not_found -> 
             raise (Error("[[InternalError]] lbl2srcp_text2emit"))
    )

(* --------------------------------------------------------------------------
   SOURCE-FILE POSITION RELATED FUNCTIONS
   1. the lexer produces int list ''nl_at'', which stores the char pos at
      which a nl was found. This is used to generate line/col info
      - for error messages
      - for the AST
      ''nl_at'' is set to [] in ''init_parse_of_file''
   2. convert positions to strings
   3. it is used the same way when a command-file is read,
      nesting of command files has to be handled
  -------------------------------------------------------------------------- *)
let pos_1line lexbuf =      (* called from lex.mll only *)
    ly.nl_at <- (Lexing.lexeme_start lexbuf)::ly.nl_at

let pos2lc c =
    let rec pos2lc = function
        | []       -> (1,c)
        | n::t     -> if c<n then pos2lc t else (2+List.length t,c-n)
    in
        pos2lc ly.nl_at

let clr_pos2lc () =
    ly.nl_at <- []

let push_pos2lc () =
    ly.nl_stack <- ly.nl_at :: ly.nl_stack

let pop_pos2lc () =
    match ly.nl_stack with
    | []   -> ly.nl_at <- []
    | h::t -> ly.nl_at <- h; ly.nl_stack <- t
