/* copyright Fraunhofer AiS; read the LICENSE  ------------------ */

#ifndef _setypes_h
#define _setypes_h

typedef void*          se_Object  ;
typedef void*          se_None    ;
typedef unsigned char  se_Bool    ;
typedef unsigned char  se_Char    ;
typedef char           se_Byte    ;
typedef float          se_Float   ;
typedef short          se_Short   ;
typedef unsigned short se_Uint16  ;
typedef int            se_Int     ;
typedef unsigned int   se_Uint32  ;
typedef long           se_Long    ;
typedef unsigned long  se_Uint64  ;
typedef double         se_Double  ;
typedef char*          se_String  ;
typedef      long      se_Time    ;

#define True  ((se_Bool)1)
#define False ((se_Bool)0)
#define Null  ((void *) 0)

#endif
