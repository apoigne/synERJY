/* ---------- tree of reactive objects -------------------------------------
conf_obj : Identity2
Sensor<double[2]>conf_obj.x : principal signal
Signal<double[2]>conf_obj.y : principal signal
   ---------- end of tree -------------------------------------------------- */
#include "se.Identity2.h"

/* ---------- global data for the run time system -------------------------- */
static tat_Identity2 conf_obj;

pat_object get_configuration (void) {
  return (pat_object) &conf_obj;
}

pat_object ct;
union t_tmpvar t__1,t__2,t__3,t__4,t__5,t__6,t__7,t__8,t__9,t__10 ;
SE_EXCEPTION_DEF

/* ---------- class SimOutput ---------------------------------------------- */
inline pat_SimOutput i_SimOutput(pat_SimOutput p__this)
{
  p__this->header.objhead.cid = c_SimOutput;
  return p__this;
}

/* ---------- configuration class Identity2 -------------------------------- */
pat_Identity2 i_Identity2(pat_Identity2 p__this)
{
  p__this->header.objhead.cid = c_Identity2;
      return p__this;
}

inline pat_Identity2 f_Identity2_Identity2_0( pat_Identity2 p__this )
{
  return p__this;
}

/* ---------- class SimInput ----------------------------------------------- */
inline pat_SimInput i_SimInput(pat_SimInput p__this)
{
  p__this->header.objhead.cid = c_SimInput;
  return p__this;
}

/* ---------- memory for ALPHA --------------------------------------------- */
EDEF(Alpha);
/* ---------- wires for internal and outputs ------------------------------- */
EDEF(S1);
/* ---------- memory ------------------------------------------------------- */
EDEF(R1);
/* ---------- wires for the result of data-expr's in ifs ------------------- */
/* ---------- wires for input; data for valued input/output signals -------- */
EDEF(I10);
EDEF(I10__n);

struct { ARRAYLEN len;se_Double a[2]; } Iv10__n;

struct { ARRAYLEN len;se_Double a[2]; } Iv10;

struct { ARRAYLEN len;se_Double a[2]; } Sv1;
/* ---------- data for valued internal signals and memories ---------------- */
/* ---------- run indices for vectors and matrices ------------------------- */
int __i,__j,__k,__b;

/* ---------- time buffer for signals, that need timestamps ---------------- */

/* ---------- read input signals buffered by Scicos ------------------------ */

void scan_input_signal (scicos_block *block)
{
  I10__n = 1;
  for (__i=1;__i>=0;__i--)
  {
    Iv10__n.a[__i] = *(se_Double*)block->inptr[0][__i];
  };
}

/* ---------- copy pending input signals to the reactive machine ----------- */

void cp_input_signal (se_Bool has_new)
{
  I10 = has_new && I10__n;
  I10__n = 0;
  if (I10)
  {
    ARRAYC1(&(Iv10),&(Iv10__n),2,sizeof(se_Double));
  }
  else
  {
    I10 = 0;
  };

}

/* ---------- function to generate outputs [of 1 INSTANT] ------------------ */
se_Time realtm, deltat;

inline se_Int Identity2_runtime_engine_instant (scicos_block *block)
{
  se_Time lasttm;

  exception = setjmp( exception_env );
  if (exception != 0)
  {    sciprint("synERJY HAS RAISED AN EXCEPTION");
    sciprint("the exception raise  is '%d'",exception);
    set_block_error(-3);
    return exception;
  };
  scan_input_signal (block);
  cp_input_signal (True);
  lasttm = realtm;
  realtm = get_scicos_time();
  if (deltat <= (se_Time)0) { deltat = realtm; };

/* ---------- signal equations --------------------------------------------- */
  ARRAYC1(&Sv1,&Iv10,2,sizeof(se_Double));
  S1 = 1;

/* ---------- output signals ----------------------------------------------- */
  { se_Double *__y = block->outptr[0];
    int  __ny  = block->outsz[0];
    for (__i = __ny; __i >=0; __i--)
    {
      __y[__i] = Sv1.a[__i];
    };
  };

/* ---------- memory equations --------------------------------------------- */
  R1 = 1;

/* ---------- copy new mem vals to mem & set alpha ------------------------- */
  Alpha = 0;
}

/* ---------- initialize the runtime engine -------------------------------- */
inline void Identity2_runtime_engine_init (scicos_block *block)
{
  target_init();
  SE_EXCEPTION_INIT
/* clear memory, set alpha */
  Alpha = 1;
  R1 = 0;
/* eqns for external signals always false */

/* initialization (if any) of static data */
  i_Identity2 (&conf_obj);
  f_Identity2_Identity2_0(&conf_obj);

/* initialization of signal declarations */
    Iv10.len = 2;
    for (__i=1;__i>=0;__i--)
    {
      Iv10.a[__i] = (se_Double) 0.0;
    };
    Sv1.len = 2;
    for (__i=1;__i>=0;__i--)
    {
      Sv1.a[__i] = (se_Double) 0.0;
    };

/* initialization of internal declarations */
}

void se_Identity2(scicos_block *block,int flag)
{
  if (flag == 4)
  { /* initialization */
    Identity2_runtime_engine_init(block);
  } else if (flag == 1)
  { /* output computation */
    Identity2_runtime_engine_instant(block);
  } else if (flag == 5)
  { /* termination */
  };
}
