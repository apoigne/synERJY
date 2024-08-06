/* copyright Fraunhofer AiS; read the LICENSE  ------------------ */

#ifndef _scan_h
#define _scan_h

#include "se_types.h"

char *pScan;

void scan_abort (char *msg);
void scan_init(void);

se_Bool   scan_bool   (void);
se_Byte   scan_byte   (void);
se_Char   scan_char   (void);
se_Short  scan_short  (void);
se_Int    scan_int    (void);
se_Long   scan_long   (void);
se_Double scan_double (void);
se_Float  scan_float  (void);
se_Time   scan_time   (void);
int       scan_String (char *name);

void scan_print_val  (unsigned char *vadr, int value_type);
int  scan_data_simul (void);

char *time2str(se_Time t);

#endif /* _scan_h */
