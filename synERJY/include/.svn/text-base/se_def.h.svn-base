/* copyright Fraunhofer AiS; read the LICENSE  ------------------ */

#ifndef _sedef_h
#define _sedef_h

#include "se_types.h"
#ifndef _DSK6713_
#ifndef CHIP_6713
#ifndef _SE_ATMEL_AVR_
#ifndef __MSP430__
#include <memory.h>
#endif
#endif
#endif
#endif
typedef struct { unsigned char   cid;
                 unsigned char   flags;
                 unsigned short  len;
                 char           *nxt;
               } t_objhead;

typedef union {
          double    align_objhead;
          t_objhead objhead;
        } tat_object, *pat_object, tat_Object, *pat_Object;

typedef int    pat_Signal, pat_ConstSignal, pat_DelayedSignal,
               pat_Flow,   pat_ConstFlow;

union t_tmpvar {
  int        i;
  pat_Object p;
};

#define SET_TRUE(w,b) (w |= (1<<b))
#define SET_FALSE(w,b) (w &= ~(1<<b))
#define TEST_VAL(w,b) (w & (1<<b))
#define SET_VAL(lhsw,lhsb,rhsw,rhsb) if (TEST_VAL(rhsw,rhsb)) { SET_TRUE(lhsw,lhsb); } else { SET_FALSE (lhsw,lhsb); };

#define EDEF(n) static unsigned char n

#define LABEL(label) label:
#define GOTO(label) goto label
#define IF_TRUE(w,b,label) if (w & (1<<b)) goto label
#define IF_FALSE(w,b,label) if (!(w & (1<<b))) goto label

/* -------------------------------------------------------------------------
 *    ARRAYs
 * ------------------------------------------------------------------------- */
typedef double ARRAYLEN;
typedef struct {ARRAYLEN len;char a;}                tat_array1, *pat_array1,
                                                     tat_vector, *pat_vector;
typedef struct {ARRAYLEN len1;ARRAYLEN len2;char a;} tat_array2, *pat_array2,
                                                     tat_matrix, *pat_matrix;

#define ARRAYI1(p,l,sz) \
(memset(p,sizeof(ARRAYLEN)+l*sz,0),((pat_array1)p)->len=l)

#define ARRAYI2(p,l1,l2,sz) \
(memset(p,2*sizeof(ARRAYLEN)+l1*l2*sz,0),\
((pat_array2)p)->len1=l1,((pat_array2)p)->len2=l2)

#define ARRAYM1(l,s) (pat_object)(ct=(pat_object)malloc(sizeof(ARRAYLEN)+l*s),\
memset(ct,sizeof(ARRAYLEN)+l*s,0),((pat_array1)ct)->len=l,ct)

#define ARRAYM2(l1,l2,s) (pat_object)\
(ct=(pat_object)malloc(2*sizeof(ARRAYLEN)+l1*l2*s),\
memset(ct,2*sizeof(ARRAYLEN)+l1*l2*s,0),\
((pat_array2)ct)->len1=l1,((pat_array2)ct)->len2=l2,ct)

#define ARRAYC1(t,f,n,l) memcpy((char *)t,(char *)f,n*l+sizeof(ARRAYLEN))

#define ARRAYC2(t,f,n1,n2,l) memcpy((char *)t,\
(char *)f,n1*n2*l+2*sizeof(ARRAYLEN))

/* -------------------------------------------------------------------------
 *    convolution
 * ------------------------------------------------------------------------- */

typedef struct {int pt;pat_array1 b;}  tat_buffer,  *pat_buffer;

/* -------------------------------------------------------------------------
 *    class BOOL
 * ------------------------------------------------------------------------- */
extern const char bool_true[];
extern const char bool_false[];

#define f_bool_not(cu)        ((se_Bool)(!(se_Bool)(cu)))

#define f_bool_to_bool(cu)    (cu)
#define f_bool_to_char(cu)    ((se_Char)(cu))
#define f_bool_to_byte(cu)    ((se_Byte)(cu))
#define f_bool_to_short(cu)   ((se_Short)(cu))
#define f_bool_to_int16(cu)   ((se_Short)(cu))
#define f_bool_to_uint16(cu)  ((se_Uint16)(cu))
#define f_bool_to_int(cu)     ((se_Int)(cu))
#define f_bool_to_int32(cu)   ((se_Int)(cu))
#define f_bool_to_uint32(cu)  ((se_Uint32)(cu))
#define f_bool_to_long(cu)    ((se_Long)(cu))
#define f_bool_to_int64(cu)   ((se_Long)(cu))
#define f_bool_to_uint64(cu)  ((se_Uint64)(cu))
#define f_bool_to_float(cu)   ((se_Float)(cu))
#define f_bool_to_double(cu)  ((se_Double)(cu))
#define f_bool_to_string(cu)  ((se_Char)(cu?bool_true:bool_false))
#define f_bool_to_time(cu)    ((se_Time)cu)

/* -------------------------------------------------------------------------
 *    class se_Char
 * ------------------------------------------------------------------------- */
se_String char_to_string (se_Int);

#define f_char_is_digit(cu) (se_Bool)(((cu)>='0' && (cu)<='9') ? True : False )
#define f_char_is_lower(cu) (se_Bool)(((cu)>='a' && (cu)<='z') ? True : False )
#define f_char_is_upper(cu) (se_Bool)(((cu)>='A' && (cu)<='Z') ? True : False )

#define f_char_to_bool(cu)  ((se_Bool)(((se_Char)(cu)!=(se_Char)0)?True:False))
#define f_char_to_char(cu)    (cu)
#define f_char_to_byte(cu)    ((se_Byte)(cu))
#define f_char_to_short(cu)   ((se_Short)(cu))
#define f_char_to_int16(cu)   ((se_Short)(cu))
#define f_char_to_uint16(cu)  ((se_Uint16)(cu))
#define f_char_to_int(cu)     ((se_Int)(cu))
#define f_char_to_int32(cu)   ((se_Int)(cu))
#define f_char_to_uint32(cu)  ((se_Uint32)(cu))
#define f_char_to_long(cu)    ((se_Long)(cu))
#define f_char_to_int64(cu)   ((se_Long)(cu))
#define f_char_to_uint64(cu)  ((se_Uint64)(cu))
#define f_char_to_float(cu)   ((se_Float)(cu))
#define f_char_to_double(cu)  ((se_Double)(cu))
#define f_char_to_string(cu)  (char_to_string((se_Double)cu))
#define f_char_to_time(cu)    ((se_Time)cu)

/* -------------------------------------------------------------------------
 *    class se_Byte
 * ------------------------------------------------------------------------- */
#define f_byte_high(cu)             ((se_Byte)((se_Byte)(cu) >> 4))
#define f_byte_low(cu)              ((se_Byte)((se_Byte)(cu) &  0x0f))

#define f_byte_to_bool(cu)          ((se_Bool)(cu))
#define f_byte_to_char(cu)          ((se_Char)(cu))
#define f_byte_to_byte(cu)          (cu)
#define f_byte_to_short(cu)         ((se_Short)(cu))
#define f_byte_to_int16(cu)         ((se_Short)(cu))
#define f_byte_to_uint16(cu)        ((se_Uint16)(cu))
#define f_byte_to_int(cu)           ((se_Int)(cu))
#define f_byte_to_int32(cu)         ((se_Int)(cu))
#define f_byte_to_uint32(cu)        ((se_Uint32)(cu))
#define f_byte_to_long(cu)          ((se_Long)(cu))
#define f_byte_to_int64(cu)         ((se_Long)(cu))
#define f_byte_to_uint64(cu)        ((se_Uint64)(cu))

/* -------------------------------------------------------------------------
 *    class se_Short
 * ------------------------------------------------------------------------- */
#define f_short_to_bool(cu)   ((se_Bool)(((se_Short)(cu)!=(se_Short)0)?True:False))
#define f_short_to_char(cu)   ((se_Char)(cu))
#define f_short_to_byte(cu)   ((se_Byte)(cu))
#define f_short_to_short(cu)  (cu)
#define f_short_to_int16(cu)  (cu)
#define f_short_to_uint16(cu) ((se_Uint16)(cu))
#define f_short_to_int(cu)    ((se_Int)(cu))
#define f_short_to_int32(cu)  ((se_Int)(cu))
#define f_short_to_uint32(cu) ((se_Uint32)(cu))
#define f_short_to_long(cu)   ((se_Long)(cu))
#define f_short_to_int64(cu)  ((se_Long)(cu))
#define f_short_to_uint64(cu) ((se_Uint64)(cu))
#define f_short_to_float(cu)  ((se_Float)(cu))
#define f_short_to_double(cu) ((se_Double)(cu))
#define f_short_to_string(cu) (int_to_string((se_Int)cu))
#define f_short_to_time(cu)   ((se_Time)(cu))

/* -------------------------------------------------------------------------
 *    class se_Uint16
 * ------------------------------------------------------------------------- */
#define f_uint16_high(cu)      ((se_Byte)((se_Uint16)(cu) >> 8))
#define f_uint16_low(cu)       ((se_Byte)((se_Uint16)(cu)))

#define f_uint16_to_bool(cu)   ((se_Bool)(((se_Uint16)(cu)!=(se_Uint16)0)?True:False))
#define f_uint16_to_char(cu)   ((se_Char)(cu))
#define f_uint16_to_byte(cu)   ((se_Byte)(cu))
#define f_uint16_to_short(cu)  ((se_Short)(cu))
#define f_uint16_to_int16(cu)  ((se_Short)(cu))
#define f_uint16_to_uint16(cu) (cu)
#define f_uint16_to_int(cu)    ((se_Int)(cu))
#define f_uint16_to_int32(cu)  ((se_Int)(cu))
#define f_uint16_to_uint32(cu) ((se_Uint32)(cu))
#define f_uint16_to_long(cu)   ((se_Long)(cu))
#define f_uint16_to_int64(cu)  ((se_Long)(cu))
#define f_uint16_to_uint64(cu) ((se_Uint64)(cu))
#define f_uint16_to_float(cu)  ((se_Float)(cu))
#define f_uint16_to_double(cu) ((se_Double)(cu))
#define f_uint16_to_string(cu) (int_to_string((se_Int)cu))
#define f_uint16_to_time(cu)   ((se_Time)(cu))

/* -------------------------------------------------------------------------
 *    class se_Int se_Int32
 * ------------------------------------------------------------------------- */
se_String int_to_string (se_Int);

#define f_int_to_bool(cu)     ((se_Bool)(((se_Int)(cu)!=(se_Int)0)?True:False))
#define f_int_to_char(cu)     ((se_Char)(cu))
#define f_int_to_byte(cu)     ((se_Byte)(cu))
#define f_int_to_short(cu)    ((se_Short)(cu))
#define f_int_to_int16(cu)    ((se_Short)(cu))
#define f_int_to_int(cu)      (cu)
#define f_int_to_int32(cu)    (cu)
#define f_int_to_uint32(cu)   ((se_Uint32)(cu))
#define f_int_to_long(cu)     ((se_Long)(cu))
#define f_int_to_int64(cu)    ((se_Long)(cu))
#define f_int_to_uint64(cu)   ((se_Uint64)(cu))
#define f_int_to_float(cu)    ((se_Float)(cu))
#define f_int_to_double(cu)   ((se_Double)(cu))
#define f_int_to_string(cu)   (int_to_string((se_Int)cu))
#define f_int_to_time(cu)     ((se_Time)(cu))

/* -------------------------------------------------------------------------
 *    class se_Uint32
 * ------------------------------------------------------------------------- */
#define f_uint32_to_bool(cu)     ((se_Bool)(((se_Uint32)(cu)!=(se_Uint32)0)?True:False))
#define f_uint32_to_char(cu)     ((se_Char)(cu))
#define f_uint32_to_byte(cu)     ((se_Byte)(cu))
#define f_uint32_to_short(cu)    ((se_Short)(cu))
#define f_uint32_to_int16(cu)    ((se_Short)(cu))
#define f_uint32_to_int(cu)      ((se_Int)(cu))
#define f_uint32_to_int32(cu)    ((se_Int)(cu))
#define f_uint32_to_uint32(cu)   (cu)
#define f_uint32_to_long(cu)     ((se_Long)(cu))
#define f_uint32_to_int64(cu)    ((se_Long)(cu))
#define f_uint32_to_uint64(cu)   ((se_Uint64)(cu))
#define f_uint32_to_float(cu)    ((se_Float)(cu))
#define f_uint32_to_double(cu)   ((se_Double)(cu))
#define f_uint32_to_string(cu)   (int_to_string((se_Int)cu))
#define f_uint32_to_time(cu)     ((se_Time)(cu))

/* -------------------------------------------------------------------------
 *    class se_Long se_Int64
 * ------------------------------------------------------------------------- */
se_String long_to_string (se_Long);

#define f_long_to_bool(cu)     ((se_Bool)(((se_Long)(cu)!=(se_Long)0)?True:False))
#define f_long_to_char(cu)     ((se_Char)(cu))
#define f_long_to_byte(cu)     ((se_Byte)(cu))
#define f_long_to_short(cu)    ((se_Short)(cu))
#define f_long_to_int16(cu)    ((se_Short)(cu))
#define f_long_to_uint16(cu)   ((se_Uint16)(cu))
#define f_long_to_int(cu)      ((se_Int)(cu))
#define f_long_to_int32(cu)    ((se_Int)(cu))
#define f_long_to_uint32(cu)   ((se_Uint32)(cu))
#define f_long_to_long(cu)     (cu)
#define f_long_to_int64(cu)    (cu)
#define f_long_to_uint64(cu)   ((se_Uint64)(cu))
#define f_long_to_float(cu)    ((se_Float)(cu))
#define f_long_to_double(cu)   ((se_Double)(cu))
#define f_long_to_string(cu)   (long_to_string((se_Long)cu))
#define f_long_to_time(cu)     ((se_Time)(cu))

/* -------------------------------------------------------------------------
 *    class se_Uint64
 * ------------------------------------------------------------------------- */
#define f_uint64_to_bool(cu)     ((se_Bool)(((se_Uint64)(cu)!=(se_Uint64)0)?True:False))
#define f_uint64_to_char(cu)     ((se_Char)(cu))
#define f_uint64_to_byte(cu)     ((se_Byte)(cu))
#define f_uint64_to_short(cu)    ((se_Short)(cu))
#define f_uint64_to_int16(cu)    ((se_Short)(cu))
#define f_uint64_to_uint16(cu)   ((se_Uint16)(cu))
#define f_uint64_to_int(cu)      ((se_Int)(cu))
#define f_uint64_to_int32(cu)    ((se_Int)(cu))
#define f_uint64_to_uint32(cu)   ((se_Uint32)(cu))
#define f_uint64_to_long(cu)     ((se_Long)(cu))
#define f_uint64_to_int64(cu)    ((se_Long)(cu))
#define f_uint64_to_uint64(cu)   (cu)
#define f_uint64_to_float(cu)    ((se_Float)(cu))
#define f_uint64_to_double(cu)   ((se_Double)(cu))
#define f_uint64_to_string(cu)   (long_to_string((se_Uint64)cu))
#define f_uint64_to_time(cu)     ((se_Time)(cu))

/* -------------------------------------------------------------------------
 *    class se_Float
 * ------------------------------------------------------------------------- */
se_String float_to_string (se_Int);

#define f_float_to_bool(cu)   ((se_Bool)(((se_Float)(cu)!=(se_Float)0)?True:False))
#define f_float_to_char(cu)   ((se_Char)(cu))
#define f_float_to_byte(cu)   ((se_Byte)(cu))
#define f_float_to_short(cu)  ((se_Short)(cu))
#define f_float_to_int16(cu)  ((se_Short)(cu))
#define f_float_to_uint16(cu) ((se_Uint16)(cu))
#define f_float_to_int(cu)    ((se_Int)(cu))
#define f_float_to_int32(cu)  ((se_Int)(cu))
#define f_float_to_uint32(cu) ((se_Uint32)(cu))
#define f_float_to_long(cu)   ((se_Long)(cu))
#define f_float_to_int64(cu)  ((se_Long)(cu))
#define f_float_to_uint64(cu) ((se_Uint64)(cu))
#define f_float_to_float(cu)  (cu)
#define f_float_to_double(cu) ((se_Double)(cu))
#define f_float_to_string(cu) (float_to_string((se_Int)cu))
#define f_float_to_time(cu)   ((se_Time)(cu))

/* -------------------------------------------------------------------------
 *    class se_Double
 * ------------------------------------------------------------------------- */
se_String double_to_string (se_Int);

#define f_double_to_bool(cu)   ((se_Bool)(((se_Double)(cu)!=(se_Double)0)?True:False))
#define f_double_to_char(cu)   ((se_Char)(cu))
#define f_double_to_byte(cu)   ((se_Byte)(cu))
#define f_double_to_short(cu)  ((se_Short)(cu))
#define f_double_to_int16(cu)  ((se_Short)(cu))
#define f_double_to_uint16(cu) ((se_Uint16)(cu))
#define f_double_to_int(cu)    ((se_Int)(cu))
#define f_double_to_int32(cu)  ((se_Int)(cu))
#define f_double_to_uint32(cu) ((se_Uint32)(cu))
#define f_double_to_long(cu)   ((se_Long)(cu))
#define f_double_to_int64(cu)  ((se_Long)(cu))
#define f_double_to_uint64(cu) ((se_Uint64)(cu))
#define f_double_to_float(cu)  ((se_Float)(cu))
#define f_double_to_double(cu) (cu)
#define f_double_to_string(cu) (double_to_string((se_Double)cu))
#define f_double_to_time(cu)   ((se_Time)cu)

/* -------------------------------------------------------------------------
 *    class se_String
 * ------------------------------------------------------------------------- */
se_Bool   str_to_bool     (se_String);
se_Char   str_to_char     (se_String);
se_Byte   str_to_byte     (se_String);
se_Long   str_to_long     (se_String);
se_Uint64 str_to_ulong    (se_String);
se_Double str_to_double   (se_String);
se_Time   str_to_time     (se_String);

#define f_string_lt(cu,p)      ((se_Bool) (strcmp(cu,p) <  0))
#define f_string_le(cu,p)      ((se_Bool) (strcmp(cu,p) <= 0))
#define f_string_gt(cu,p)      ((se_Bool) (strcmp(cu,p) >  0))
#define f_string_ge(cu,p)      ((se_Bool) (strcmp(cu,p) >= 0))
#define f_string_eq(cu,p)      ((se_Bool) (strcmp(cu,p) == 0))
#define f_string_ne(cu,p)      ((se_Bool) (strcmp(cu,p) != 0))
#define f_string_length(cu)    ((se_Int)   (strlen(cu)))

#define f_string_to_bool(cu)   ((se_Bool)  str_to_bool  ((se_String) cu))
#define f_string_to_char(cu)   ((se_Char)  str_to_char  ((se_String) cu))
#define f_string_to_byte(cu)   ((se_Byte)  str_to_byte  ((se_String) cu))
#define f_string_to_short(cu)  ((se_Short) str_to_long  ((se_String) cu))
#define f_string_to_int16(cu)  ((se_Short) str_to_long  ((se_String) cu))
#define f_string_to_uint16(cu) ((se_Uint16)str_to_ulong ((se_String) cu))
#define f_string_to_int(cu)    ((se_Int)   str_to_long  ((se_String) cu))
#define f_string_to_int32(cu)  ((se_Int)   str_to_long  ((se_String) cu))
#define f_string_to_uint32(cu) ((se_Uint32)str_to_ulong ((se_String) cu))
#define f_string_to_long(cu)   (str_to_long          ((se_String) cu))
#define f_string_to_int64(cu)  (str_to_long          ((se_String) cu))
#define f_string_to_uint64(cu) (str_to_ulong         ((se_String) cu))
#define f_string_to_double(cu) ((se_Double)str_to_double((se_String) cu))
#define f_string_to_float(cu)  ((se_Float) str_to_double((se_String) cu))
#define f_string_to_string(cu) (cu)
#define f_string_to_time(cu)   ((se_Time)  str_to_time  ((se_String) cu))

/* -------------------------------------------------------------------------
 *    class TIME
 * ------------------------------------------------------------------------- */
#if defined(se__unix) || defined(se__cygwin)
#include <unistd.h>
#endif

se_Time   tp_make (void);
se_Time   tp_mk_microsec (se_Int);
se_Time   tp_mk_millisec (se_Int);
se_Time   tp_mk_sec (se_Int);
se_Time   tp_mk_min (se_Int);
se_Time   tp_mk_hour (se_Int);
se_Time   tp_add (se_Time,se_Time);
se_Time   tp_delta (se_Time,se_Time);
se_String time_to_string (se_Time);

#define f_time_lt(cu,p)         (cu < p)
#define f_time_le(cu,p)         (cu <= p)
#define f_time_gt(cu,p)         (cu > p)
#define f_time_ge(cu,p)         (cu >= p)
#define f_time_add(cu,p)        (tp_add (cu,p))
#define f_time_sub(cu,p)        (tp_delta (cu,p))

#define f_time_mikrosec(p)      (tp_mk_microsec (p))
#define f_time_millisec(p)      (tp_mk_millisec (p))
#define f_time_sec(p)           (tp_mk_sec      (p))
#define f_time_min(p)           (tp_mk_min      (p))
#define f_time_hour(p)          (tp_mk_hour     (p))
#define f_time_zero()           (tp_make ())

#define f_time_to_bool(cu)      ((se_Bool)(((se_Time)(cu) != (se_Time)0)?True:False))
#define f_time_to_char(cu)      ((se_Char)(cu))
#define f_time_to_byte(cu)      ((se_Byte)(cu))
#define f_time_to_short(cu)     ((se_Short)(cu))
#define f_time_to_int16(cu)     ((se_Short)(cu))
#define f_time_to_uint16(cu)    ((se_Uint16)(cu))
#define f_time_to_int(cu)       ((se_Int)(cu))
#define f_time_to_int32(cu)     ((se_Int)(cu))
#define f_time_to_uint32(cu)    ((se_Uint32)(cu))
#define f_time_to_long(cu)      ((se_Long)(cu))
#define f_time_to_int64(cu)     ((se_Long)(cu))
#define f_time_to_uint64(cu)    ((se_Uint64)(cu))
#define f_time_to_float(cu)     ((se_Float)(cu))
#define f_time_to_double(cu)    ((se_Double)(cu))
#define f_time_to_string(cu)    (time_to_string((se_Time)cu))
#define f_time_to_time(cu)      (cu)

#endif /* _sedef_h */
