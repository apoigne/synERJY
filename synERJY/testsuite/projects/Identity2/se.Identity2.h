#ifndef _se_configuration_h
#define _se_configuration_h

#include <se_def.h>
#include <se_rt.h>

#include <routines/scicos/scicos_block.h>
#include <routines/machine.h>


extern se_Time __deltat;

#define TIMING __deltat

inline void Identity2_runtime_engine_init (scicos_block *block);
inline se_Int Identity2_runtime_engine_instant (scicos_block *block);

/* ---------- typedef, decl, class-id for class SimOutput ------------------ */
typedef struct at_SimOutput tat_SimOutput, *pat_SimOutput;
pat_SimOutput i_SimOutput (pat_SimOutput);
#define c_SimOutput 5

/* ---------- typedef, decl, class-id for class Identity2 ------------------ */
typedef struct at_Identity2 tat_Identity2, *pat_Identity2;
pat_Identity2 i_Identity2 (pat_Identity2);
#define c_Identity2 4

/* ---------- typedef, decl, class-id for class SimInput ------------------- */
typedef struct at_SimInput tat_SimInput, *pat_SimInput;
pat_SimInput i_SimInput (pat_SimInput);
#define c_SimInput 3

/* ---------- typedef, decl, class-id for class Input ---------------------- */
typedef struct at_Input tat_Input, *pat_Input;
pat_Input i_Input (pat_Input);
#define c_Input 2

/* ---------- typedef, decl, class-id for class Output --------------------- */
typedef struct at_Output tat_Output, *pat_Output;
pat_Output i_Output (pat_Output);
#define c_Output 1

/* ---------- field-struct, method header for class Output ----------------- */
struct at_Output
{
  tat_object header;
};


/* ---------- field-struct, method header for class Input ------------------ */
struct at_Input
{
  tat_object header;
};


/* ---------- field-struct, method header for class SimInput --------------- */
struct at_SimInput
{
  tat_object header;
};


/* ---------- field-struct, method header for class Identity2 -------------- */
struct at_Identity2
{
  tat_object header;
};

inline pat_Identity2 f_Identity2_Identity2_0( pat_Identity2 p__this );

/* ---------- field-struct, method header for class SimOutput -------------- */
struct at_SimOutput
{
  tat_object header;
};


/* ---------- static fields of primitive type for all classes -------------- */
#define sta_Identity2_dt_used_in_first_instant_exception 9
#define sta_Identity2_class_cast_exception 8
#define sta_Identity2_timestamp_wait_too_long_exception 7
#define sta_Identity2_instant_caused_time_overflow_exception 6
#define sta_Identity2_out_of_memory_exception 5
#define sta_Identity2_array_negative_size_exception 4
#define sta_Identity2_array_index_out_of_bounds_exception 3
#define sta_Identity2_null_pointer_exception 2
#define sta_Identity2_throw_value_is_zero_exception 1
#define sta_Identity2_timing 0

/* ---------- no dynamic creation of objects required ---------------------- */

/* ---------- defines related to arrays ------------------------------------ */
#define check_pointer(ptr,lbl)\
      (ptr==Null && (mkExc(2,lbl),0))
#define check_index(ptr,i,lbl)\
      ((i < 0 ||i >= ((pat_array1)ptr)->len) && (mkExc(3,lbl),0))
#define check_indices(ptr,i,j,lbl)\
      ((i < 0 || j < 0 || i >= ((pat_array2)ptr)->len1 || j >= ((pat_array2)ptr)->len2) && (mkExc(3,lbl),0))


#endif /* _se_configuration_h */
