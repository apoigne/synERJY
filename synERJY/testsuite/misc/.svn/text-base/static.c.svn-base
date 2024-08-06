/* copyright Fraunhofer AiS, Team SYNC; read the LICENSE  ------------------ */

#include <stdio.h>

int static_c_int;

void setIt() {
  static_c_int = 1;
}

void tstIt(int i) {
  if (static_c_int == i) {
     printf("ok\n");
  } else {
     printf("error\n");
  };
}
