/* copyright Fraunhofer AiS; read the LICENSE  ------------------ */

#ifndef SETRANSFER_H
#define SETRANSFER_H

/* ------------------------------------------------------------------------ */
#if defined(se__unix) || defined(se__cygwin)
typedef void *seTransfer_t, **seTransfer_tp;
typedef void *seThread_t,   **seThread_tp;

#elif defined(se__win32) || defined (se__msvc) || defined (se__msys)

    /* see winnt.h : typedef void *HANDLE
     * see windef.h
     *    DWORD, LONG, HANDLE
     * see winnt.h
     *    PRTL_CRITICAL_SECTION  and  RTL_CRITICAL_SECTION
     * winbase.h
     *    CRITICAL_SECTION
     */
typedef void* seEvent_t;

typedef struct {
    seEvent_t produced;
    seEvent_t consumed;
    volatile int aborted;
} seTransfer_t, *seTransfer_tp;

typedef unsigned long seThread_t, *seThread_tp;

void sleep (long sec);
#else
#error  se__* symbol missing or unexpected
#endif
/* ------------------------------------------------------------------------ */

void seThread_create(seThread_tp psethread, void *(*routine)(void *));
void seThread_end();

void seTransfer_create(seTransfer_tp ptrans);
void seTransfer_abort(seTransfer_tp ptrans);
int  seTransfer_isAborted(seTransfer_tp ptrans);

void seTransfer_consumed(seTransfer_tp ptrans);
void seTransfer_produced(seTransfer_tp ptrans);
int  seTransfer_isConsumed(seTransfer_tp ptrans);
int  seTransfer_isProduced(seTransfer_tp ptrans);
void seTransfer_awaitConsumed(seTransfer_tp ptrans);
void seTransfer_awaitProduced(seTransfer_tp ptrans);

#endif  /* SETRANSFER_H */
