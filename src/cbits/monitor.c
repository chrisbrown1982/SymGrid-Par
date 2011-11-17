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
 *
 * ATTENTION: C calls are not atomic!!! We only store MVars here .
 * -------------------------------------------------------------------------*/
#include <stdio.h> /* NULL pointer */

#include "monitor.h"

/* Pointers to be saved */

/* this one saves a server */
static void* mvarp=NULL;
/* this one stores unique IDs */
static void* mvarp2=NULL; /* :: MVar Integer */


/* server */
void cwriteMVarPointer (void *mvarpp) { mvarp=mvarpp; }

/* delete values after stopping, so we avoid double usage */
void cdeleteMVarPointer() { mvarp=NULL; }

void *creadMVarPointer() { return mvarp; }

/* unique ID MVar */
void cwriteIDVarPointer (void *mvarpp) { mvarp2=mvarpp; }

/* delete values after stopping, so we avoid double usage */
void cdeleteIDVarPointer() { mvarp2=NULL; }

void *creadIDVarPointer() { return mvarp2; }

