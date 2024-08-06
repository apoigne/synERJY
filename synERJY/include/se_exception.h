#include <se_types.h>

se_Int mkExc( se_Int e, se_Int l );

/* Attention: If changed the corresponding matches in
   exception2string in sim_util.ml must be changed
*/
#define SE_ZERO_EXCEPTION 1
#define SE_NULL_POINTER_EXCEPTION 2
#define SE_ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION 3
#define SE_ARRAY_NEGATIVE_SIZE_EXCEPTION 4
#define SE_OUT_OF_MEMORY_EXCEPTION 5
#define SE_TIME_OVERFLOW_EXCEPTION 6
#define SE_TIMESTAMP_OVERFLOW_EXCEPTION 7
#define SE_CLASS_CAST_EXCEPTION 8
#define SE_DELTAT_IN_FIRST_INSTANT_EXCEPTION 9

#define SE_EXCEPTION_DEF \
    static se_Int exception; \
    static se_Int exception_info; \
    static jmp_buf exception_env; \
    \
    se_Int mkExc( se_Int e, se_Int l ) { \
    exception_info = l; \
    /* the id 1 inserted for exception number 0 must not be changed */ \
    longjmp(exception_env,(e==0)?1:(int)e); \
    return 0; \
    }

#define SE_EXCEPTION_CATCH \
  exception = setjmp( exception_env ); \
  if (exception != 0) return exception;

#define SE_EXCEPTION_INIT exception=0; 
