/* copyright Fraunhofer AiS; read the LICENSE  ------------------ */

#ifndef _SE_TIME_H_
#define _SE_TIME_H_

#include "se_types.h"

se_Time tp_make (void);

se_Time tp_mk_microsec (se_Int p);

se_Time tp_mk_millisec (se_Int p);

se_Time tp_mk_sec (se_Int p);

se_Time tp_mk_min (se_Int p);

se_Time tp_mk_hour (se_Int p);

se_Bool tp_lt (se_Time p, se_Time q);
se_Bool tp_eq (se_Time p, se_Time q);
se_Bool tp_le (se_Time p, se_Time q);
se_Bool tp_gt (se_Time p, se_Time q);
se_Bool tp_ge (se_Time p, se_Time q);

se_Time tp_add (se_Time cu, se_Time p);

se_Time tp_delta (se_Time cu, se_Time p);

#endif  /* _SE_TIME_H */
