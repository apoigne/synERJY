#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>


/* ==========================================================================
   print routines for debugging
   ========================================================================== */
#include <stdio.h>

#define TR trace_nr(__LINE__)
#define TRN(n) trace_nr(n)

void trace_nr(int nr)
{
    printf("*** trace-info %d\n",nr);
    fflush(stdout);
}
void trace_msg(char* msg)
{
    printf("*** trace-msg  %s\n",msg);
    fflush(stdout);
}
/* ==========================================================================
   data for ocaml <--> tcl/tk
   ========================================================================== */
static Tcl_Interp *interp         = NULL;
static Tk_Window  main_window     = NULL;
static value      *tk2ocaml2tk    = NULL;  /* closure */
static value      *timer2ocaml2tk = NULL;  /* closure */

/* ==========================================================================
   called when the tcl/tk command "ocaml" is executed
   argv[0] is the command's name ("ocaml"),
   argv[1] is the name of the ocaml callback (name),
   the rest are arguments to the ocaml callback (args).
   ========================================================================== */
int tk2ocaml (ClientData clientdata, Tcl_Interp *interp, int argc, char **argv)
{
  /* should be safe, even if not declared */
  value result;

  /* get the closure of tk2ocaml2tk, registered in tk.ml */
  if (tk2ocaml2tk == NULL) {
     tk2ocaml2tk = caml_named_value ("tk2ocaml2tk");
  };

  /* Call the dispatcher */
  result = callback2 (*tk2ocaml2tk,
                      copy_string(argv[1]),
                      copy_string_array(argv+2));

  /* Decode result which is either "" (OK) or "..." (ERROR text)
     TclTk expects return-value == 0 if OK, and return-value > 0 if error */
  Tcl_SetResult (interp,String_val(result),TCL_VOLATILE);
  return 0; /* (strcmp(String_val(result),"") != 0); */
}

/* ==========================================================================
   the timer handler [called after TIMEOUT millisec]
   Set up when c_RegisterTimer is called
   if the timer expires, timer2ocaml2tk is called, afterwards the
   timer handler is set up again (and will be called as long as Tcl/Tk runs
   ========================================================================== */
#define TIMEOUT 100

void timer2ocaml (ClientData clientdata)
{
  /* should be safe, even if not declared */
  value result;

  /* get the closure of timer2ocaml2tk, registered in tk.ml */
  if (timer2ocaml2tk == NULL) {
     timer2ocaml2tk = caml_named_value ("timer2ocaml2tk");
  };

  /* Call the dispatcher */
  result = callback (*timer2ocaml2tk,0);
  Tcl_CreateTimerHandler(TIMEOUT,timer2ocaml,(ClientData) NULL);

  return;

}
/* ==========================================================================
   functions starting with c_ are called from Ocaml. This is either
   1. after Ocamls startup (to create windows, frames, to pack etc or
   2. to initiate the event processing or
   3. during a callback from Tk to Ocaml (the most common case)
   ========================================================================== */

/* ==========================================================================
   (1) SETUP TCL/TK ENVIRONMENT
   ========================================================================== */
value c_MainWindow (value display, value window_name, value class_name,
                    value argv0)
{
  CAMLparam4 (display,window_name,class_name,argv0);

  Tcl_FindExecutable(String_val(argv0));

  /* Create an interpreter */
  interp = Tcl_CreateInterp();

  /* This is required by "unknown" and thus autoload */
  Tcl_SetVar (interp,"tcl_interactive","0",TCL_GLOBAL_ONLY);

  /* Initialize Tcl and Tk */
  if (Tcl_Init(interp) != TCL_OK) {
      failwith (interp->result);
  };
  if (Tk_Init(interp)  != TCL_OK) {
      failwith (interp->result);
  };

  /* Register the "ocaml" command */
  Tcl_CreateCommand (interp,
                     "ocaml",
                     tk2ocaml, 
                     (ClientData) NULL,
                     (Tcl_CmdDeleteProc *)NULL);
  /* Open main window */
  main_window = Tk_MainWindow(interp);
  if (main_window == NULL) { failwith (interp->result); };

  Tk_GeometryRequest (main_window, 200, 200);

  CAMLreturn (Val_unit);
}

/* ==========================================================================
   (1) CREATE TIMER HANDLER (WHICH OPERATES FOREVER)
   ========================================================================== */
value c_CreateTimer (value dummy)
{
  CAMLparam1 (dummy);
  Tcl_CreateTimerHandler(TIMEOUT,timer2ocaml,(ClientData) NULL);
  CAMLreturn (Val_unit);
}

/* ==========================================================================
   (2) START TCL/TK MAINLOOP
   ========================================================================== */
value c_MainLoop (value dummy)
{
  CAMLparam1 (dummy);
  Tk_MainLoop();
  CAMLreturn (Val_unit);
}

/* ==========================================================================
   (2) PROCESS ONE TCL/TK EVENT
   ========================================================================== */
value c_OneEvent (value dummy)
{
  CAMLparam1 (dummy);
  Tcl_DoOneEvent(0);
  CAMLreturn (Val_unit);
}

/* ==========================================================================
   (3) EVALUATE A TK-COMMAND
   ========================================================================== */
value c_TkEval(value v)
{
  int i;
  int argc;
  char **argv;
  int result;
  Tcl_CmdInfo info;

  CAMLparam1 (v);
  if (!interp) failwith ("Tcl/Tk not initialised");

  /* allocate argv */
  argc = Wosize_val(v);
  argv = (char **)stat_alloc ((argc+1)*sizeof(char *));
  argv[argc] = NULL;

  /* copy the Tcl/Tk command */
  for (i = 0; i < argc; i++) {
      argv[i] = String_val (Field(v,i));
  }

  /* interpret the command */
  Tcl_ResetResult(interp);
  if (Tcl_GetCommandInfo (interp,argv[0],&info)) {
     /* command found */
     result = (*info.proc)(info.clientData,interp,argc,argv);
  } else {
     /* unknown command, try "unknown" instead: it may implement autoload  */
     if (Tcl_GetCommandInfo (interp,"unknown",&info)) {
        /* "unknown" found, so shift arguments and interpret "unknown" */
        for (i = argc+1; i > 0; i--) argv[i] = argv[i-1];
        argv[0] = "unknown";
        result = (*info.proc)(info.clientData,interp,argc+1,argv);
     } else {
        /* "unknown" not found, so give up */
        result = TCL_ERROR;
        Tcl_AppendResult (interp,"Unknown command \"",argv[0],"\"",NULL);
     };
  };

  /* free argv */
  stat_free((char *)argv);
  
  /* interpret the result */
  switch (result) {
    case TCL_OK:
      CAMLreturn (copy_string(interp->result));
    case TCL_ERROR:
      failwith(interp->result);
    default:
      failwith("unexpected Tcl result");
  }
}
