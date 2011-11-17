/* ---------------------------------------------------------------------------
 * Haskell interface to Maple
 * 
 * (c) 2000
 *     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
 *     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
 * (c) 2003
 *     Ricardo Peña-Mari <ricardo@sip.ucm.es>
 *     Rafael Martínez-Torres <rmartine@fdi.ucm.es>
 *
 * This C code has an only objective: to serve as a memory repository,
 * and simulate the "state" of the interface, hence a non functional
 * feature. It will be accessed via monad IO() 
 * We take advantage of the FFI atomically exucuted functions to simulate
 * the effect of a monitor:
 * FFI 1.0 and ghc-5.02.3 are so compliant:
 *         If no further Haskell-RTS actions are invoked inside a function, this
 *         is executed atomically, i.e, the whole STG is blocked.
 *
 * 2004-02-03:
 *         This may change on furter verions, as in ghc-6.X, 
 *         if you gives explictly a linking flag.
 * -------------------------------------------------------------------------*/
#include <stdio.h> /* NULL pointer */

/* used by Haskell for locking interface */
int lockVar=0;
/* locking queue */

/* Pointers to be saved */

static void* inrp=NULL;
static void* outwp=NULL;
static void* pidp=NULL;
static void* queuep=NULL;




/* Accesing pointers */
/* Concerning Pointers */
void cwritePointers (void *inrpp,void *outwpp,void *pidpp,void *queuepp)
{
    inrp=inrpp;
    outwp=outwpp;
    pidp=pidpp;
    queuep=queuepp;
}

void *creadInPipePointer()
{
    return inrp;
}
void *creadOutPipePointer()
{
    return outwp;
}
void *creadPidPointer()
{
    return pidp;
}

void *creadQueuePointer()
{
    return queuep;
}


/* Initializes the number of threads inside the critical region */
void cinit()
{
    lockVar = 0;
    return ;
}

/*  Returns the old value after incrementing it */
int ctestAndInc()
{
     return lockVar++;
}

/* Ditto:
   First decrement and then return new valu
*/
int ctestAndDec()
{
    return --lockVar;
    
}

