/* ---------------------------------------------------------------------------
 * Haskell interface to CA systems
 * 
 * (c) 2008 Jost Berthold <berthold@mathematik.uni-marburg.de>
 *
 * Header file to include into Monitor.hs
 * This C code saves the state of the interface, hence a non functional
 * feature. It will be accessed via monad IO() 
 *
 * Attention: we cannot assume atomicity of these calls!!!
 * -------------------------------------------------------------------------*/

void cwriteMVarPointer (void *mvarpp);
void cdeleteMVarPointer();
void *creadMVarPointer();

void cwriteIDVarPointer (void *mvarpp);
void cdeleteIDVarPointer();
void *creadIDVarPointer();

