/* copyright Fraunhofer AiS, Team SYNC; read the LICENSE  ------------------ */

#include <stdio.h>

void dbg_nl()    { fprintf(stderr,"\n"); }
void dbg_w()     { fprintf(stderr," "); }

void dbg_i(p)    { fprintf(stderr,"%ld",p); }
void dbg_inl(p)  { fprintf(stderr,"%ld\n",p); }
void dbg_wi(p)   { fprintf(stderr," %ld",p); }
void dbg_c(p)    { fprintf(stderr,"%c",p); }
void dbg_cnl(p)  { fprintf(stderr,"%c\n",p); }
void dbg_wc(p)   { fprintf(stderr," %c",p); }
void dbg_r(p)    { fprintf(stderr,"%f",p); }
void dbg_rnl(p)  { fprintf(stderr,"%f\n",p); }
void dbg_wr(p)   { fprintf(stderr," %f",p); }
void dbg_d(p)    { fprintf(stderr,"%lf",p); }
void dbg_dnl(p)  { fprintf(stderr,"%lf\n",p); }
void dbg_wd(p)   { fprintf(stderr," %lf",p); }
void dbg_by(p)   { fprintf(stderr,"%02x",p); }
void dbg_bynl(p) { fprintf(stderr,"%02x\n",p); }
void dbg_wby(p)  { fprintf(stderr," %02x",p); }

#define b2s(p)   (p?"true":"false")

void dbg_b(p)    { fprintf(stderr,"%s",b2s(p)); }
void dbg_bnl(p)  { fprintf(stderr,"%s\n",b2s(p)); }
void dbg_wb(p)   { fprintf(stderr," %s",b2s(p)); }
void dbg_s(p)    { fprintf(stderr,"%s",p); }
void dbg_snl(p)  { fprintf(stderr,"%s\n",p); }
void dbg_ws(p)   { fprintf(stderr," %s",p); }
void dbg_t(p)    { fprintf(stderr,"NYI\n"); }
void dbg_tnl(p)  { fprintf(stderr,"NYI\n"); }
void dbg_wt(p)   { fprintf(stderr,"NYI\n"); }
