open Ly
open P
open Ast
open Util

(* -------------------------------------------------------------------------
   catch exception, before they propagate to TK
   ------------------------------------------------------------------------- *)
let set_code_generator s =
    Util_gen.boolgen :=
       if s="expr-word" then Util_gen.ExprWordSty    else
       if s="expr-bit"  then Util_gen.ExprBitSty     else
       if s="jmp-bit"   then Util_gen.JmpBitStyle    else
       exit(2)

let set_command_file keyword file =
    if se.cmd_file = "" then (
       se.cmd_file  <- file;
       se.batchmode <- (keyword = "-b")
    ) else (
       P.ps "[[Error]] illegal to use both the -b and the -f \
             command line parameter\n";
       exit(1)
    )

let parse_cmdline_args () =
    Arg.parse
      [ ("-f" ,Arg.String(set_command_file "-f"),
               "<file> load command-file after $SE_HOME/.serc in an \
               interactive session");
        ("-I" ,Arg.String(fun s -> ()),
               "ignore -I command (3.07 error");
        ("-b" ,Arg.String(set_command_file "-b"),
               "<file> process commands from file, then quit. This batch mode \
               is used from Makefile's");
        ("-cg",Arg.String(set_code_generator),
               "<sty>: select code generator - INTERNAL USE FOR TESTS")
      ]
      ( function f -> se.cmdline_files <- f::se.cmdline_files )
      "se -f <file> -b <file> -cg <sty> <se-file-to-load>..."

let conf2cid () =
    match se.conf_class with
    | Typ(cid,_) -> cid
    | _          -> Err.intern "conf2str"

let conf2str () =
    Ly.c2str (conf2cid ())
(* ========================================================================== *)
let init_sa () =
    Puctrl.reset();
    se.target_sys <- Platform "darwin";
    try se.cmdline_files <- [];
        parse_cmdline_args ();
        if se.batchmode then (
        List.iter Puctrl.add_file_without_typecheck se.cmdline_files;
        Puctrl.process_cmd_file se.cmd_file;
        ) else (
           ps "Stand alone version of synERJY. The option -b \
               must be used instead of -f.\n";
           pF();
           exit 2
        )
    with _ -> exit 1;
    exit 0    

let _ = Callback.register "init_sa" init_sa
