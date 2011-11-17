#############################################################################
#
# This is straightforward implementation of Karatsuba multiplication for
# integers. It is used to demonstrate the algorithm using the GAP language,
# and not suitable for practical implementation, because it is slow by a
# number of reasons.
#
KaratsubaMultiplication:=function( x, y )
local n, b, x1, x2, y1, y2, u, v, w;
if x < 10 and y < 10  then
  return x*y;
else
  n:=Int( Length ( String ( Maximum( x, y ) ) ) / 2 );
  b := 10^n;
  x1 := Int( x / b );
  x2 := x mod b;
  y1 := Int( y / b );
  y2 := y mod b;
  u := KaratsubaMultiplication(x1,y1);
  v := KaratsubaMultiplication(x2,y2);
  w := KaratsubaMultiplication(x1+x2,y1+y2);
  return u*(b^2) + (w-u-v)*b + v;
fi; 
end;


#############################################################################
#
# This is straightforward implementation of Karatsuba multiplication for
# polynomials. It is used to demonstrate the algorithm using the GAP language,
# and not suitable for practical implementation, because it is slow by a
# number of reasons.
#
KaratsubaPolynomialMultiplicationSlow:=function( f, g )
local deg, n, x, b, f1, f0, g1, g0, u, v, w;
if IsZero(f) or IsZero(g) then 
  return Zero(f);
fi;
deg := Maximum( List( [f,g], DegreeOfLaurentPolynomial ) );
n:=1;
while n < deg do
  n:=n*2;
od;
if n=1 then
  return f*g;
else
  x := IndeterminateOfUnivariateRationalFunction( f );
  b := x^(n/2);
  f1 := EuclideanQuotient( f, b );
  f0 := EuclideanRemainder( f, b );
  g1 := EuclideanQuotient( g, b );
  g0 := EuclideanRemainder( g, b );
  u := KaratsubaPolynomialMultiplicationSlow( f1, g1 );
  v := KaratsubaPolynomialMultiplicationSlow( f0, g0 );
  w := KaratsubaPolynomialMultiplicationSlow( f1+f0, g1+g0 );
  return u*(b^2) + (w-u-v)*b + v;
fi; 
end;


#############################################################################
#
# This implementation of Karatsuba multiplication for polynomials is faster
# than the previous, because it uses internal representation of polynomials
# for fast finding Euclidean quotient and remainder, and fast multiplying
# by x^(n/2) and x^n. Nevertheless, it is not fully efficient, since it
# uses explicit polynomials as arguments in recursive calls.
#
KaratsubaPolynomialMultiplicationBetter:=function( f, g )
local deg, n, halfn, x, b, nr, fam, f1, f0, g1, g0, u, v, w, 
      cf, cf1, cf0, cg, cg1, cg0, k, pos, val,
      cu, ub2, cwuv, wuvb;
if IsZero(f) or IsZero(g) then 
  return Zero(f);
fi;
deg := Maximum( List( [f,g], DegreeOfLaurentPolynomial ) );
n:=1;
while n < deg do
  n:=n*2;
od;
if n=1 then
  return f*g;
else
  halfn := n/2;
  x := IndeterminateOfUnivariateRationalFunction( f );
  b := x^(halfn);
  nr := IndeterminateNumberOfLaurentPolynomial(f); 
  fam := FamilyObj( 1 );
  if DegreeOfLaurentPolynomial(f) >= halfn then
    cf := CoefficientsOfLaurentPolynomial( f );
    k:=halfn-cf[2]+1;
    if k<1 then
      pos:=1;
      val:=1-k;
    else
      pos:=k; 
      val:=0; 
    fi;  
    cf1 := cf[1]{[ pos .. Length(cf[1])]};
    f1 := LaurentPolynomialByCoefficients( fam, cf1, val, nr ); # EuclideanQuotient( f, b )
    cf0 := cf[1]{[ 1 .. halfn-cf[2] ]};
    f0 := LaurentPolynomialByCoefficients( fam, cf0, cf[2], nr ); # EuclideanRemainder( f, b )
  else
    f1:=Zero(f);
    f0:=f;
  fi;
  if DegreeOfLaurentPolynomial(g) >= halfn then
    cg := CoefficientsOfLaurentPolynomial( g );
    k:=halfn-cg[2]+1;
    if k<1 then
      pos:=1;
      val:=1-k;
    else
      pos:=k; 
      val:=0; 
    fi; 
    cg1 := cg[1]{[ pos .. Length(cg[1])]};
    g1 := LaurentPolynomialByCoefficients( fam, cg1, val, nr ); # EuclideanQuotient( g, b )
    cg0 := cg[1]{[ 1 .. halfn-cg[2] ]};
    g0 := LaurentPolynomialByCoefficients( fam, cg0, cg[2], nr ); # EuclideanRemainder( g, b )
  else
    g1:=Zero(g);
    g0:=g;
  fi;  
  u := KaratsubaPolynomialMultiplicationBetter(f1,g1);
  v := KaratsubaPolynomialMultiplicationBetter(f0,g0);
  w := KaratsubaPolynomialMultiplicationBetter(f1+f0,g1+g0);
  cu := CoefficientsOfLaurentPolynomial( u );
  ub2 := LaurentPolynomialByCoefficients( fam, cu[1], cu[2]+n, nr );
  cwuv := CoefficientsOfLaurentPolynomial( w-u-v );
  wuvb := LaurentPolynomialByCoefficients( fam, cwuv[1], cwuv[2]+halfn, nr );
  return ub2 + wuvb + v;
fi; 
end;

#############################################################################
#
# KARATSUBA MULTIPLICATION FOR POLYNOMIALS
#
#############################################################################

PlusLaurentPolynomialsExtRep := function( f, g )
local val, t, shift, i, j, ind, pos, pos1, k, zero;
if f[1]=[] then           # f=0 
  return g;
elif g[1]=[] then         # g=0
  return f;
elif f[2]=g[2] then       # f and g starts from monomials of the same degree
  val := f[2];
  t := f[1] + g[1];
elif f[2] < g [2] then    # f starts earlier
  zero := Zero(f[1][1]);
  val := f[2];
  t := ShallowCopy(f[1]); 
  shift := g[2]-f[2];
  for j in [ Length(t) + 1 .. shift ] do
    t[j]:=zero;
  od;
  for i in [ 1 .. Length(g[1])] do
    ind := shift + i;
    if IsBound(t[ind]) then
      t[ind] := t[ind] + g[1][i];
    else
      t[ind] := g[1][i];
    fi;   
  od;
else                      # g starts earlier
  zero := Zero(f[1][1]);
  val := g[2];
  t := ShallowCopy(g[1]);
  shift := f[2]-g[2];
  for j in [ Length(t) + 1 .. shift ] do
    t[j]:=zero;
  od;
  for i in [ 1 .. Length(f[1])] do
    ind := shift + i;
    if IsBound(t[ind]) then
      t[ind] := t[ind] + f[1][i];
    else
      t[ind] := f[1][i];
    fi; 
  od;  
fi;  
# Final analysis, removing trailing zeroes from both sides
if t=[] then
  return [ [ ], 0 ];
else
  pos := PositionNonZero( t );
  if pos = Length(t)+1 then
    return [ [ ], 0 ];
  else
    pos1 := First([1..Length(t)], k -> t[Length(t)-k+1]<>0 );
    return [ t{[pos..Length(t)-pos1+1]}, val + pos -1 ];
  fi;  
fi;  
end;


MinusLaurentPolynomialsExtRep := function( f, g )
local val, t, shift, i, j, ind, pos, pos1, k, zero;
if f[1]=[] then           # f=0
  return [ -g[1], g[2] ];
elif g[1]=[] then         # g=0
  return f;
elif f[2]=g[2] then       # f and g starts from monomials of the same degree
  val := f[2];
  t := f[1] - g[1];
elif f[2] < g [2] then    # f starts earlier
  zero := Zero(f[1][1]);
  val := f[2];
  t := ShallowCopy(f[1]);
  shift := g[2]-f[2];
  for j in [ Length(t) + 1 .. shift ] do
    t[j]:=zero;
  od;
  for i in [ 1 .. Length(g[1])] do
    ind := shift + i;
    if IsBound(t[ind]) then
      t[ind] := t[ind] - g[1][i];
    else
      t[ind] := -g[1][i];
    fi;   
  od;
else                      # g starts earlier
  zero := Zero(f[1][1]);
  val := g[2];
  t := -ShallowCopy(g[1]);
  shift := f[2]-g[2];
  for j in [ Length(t) + 1 .. shift ] do
    t[j]:=zero;
  od;
  for i in [ 1 .. Length(f[1])] do
    ind := shift + i;
    if IsBound(t[ind]) then
      t[ind] := t[ind] + f[1][i];
    else
      t[ind] := f[1][i];
    fi; 
  od;  
fi;  
# Final analysis, removing trailing zeroes from both sides
if t=[] then
  return [ [ ], 0 ];
else
  pos := PositionNonZero( t );
  if pos = Length(t)+1 then
    return [ [ ], 0 ];
  else
    pos1 := First([1..Length(t)], k -> t[Length(t)-k+1]<>0 );
    return [ t{[pos..Length(t)-pos1+1]}, val + pos - 1 ];
  fi;  
fi;  
end;

#############################################################################
# 
# Must be >=3 for correct work. May depend on the coefficients field.
# For polynomials with integer coefficients, empirically determined
# optimal value is 48.
# 
KARATSUBA_CUTOFF := 48; 

#############################################################################
#
# Here the input is the presentation of polynomials as it is produced by
# the function CoefficientsOfLaurentPolynomial, that is the polynomial
# 2*x^4+3*x^3+x^2+x+1 will be represented as [ [ 1, 1, 1, 3, 2 ], 0 ],
# and 5*x^10-2*x^8+x^6 as [ [ 1, 0, -2, 0, 5 ], 6 ]
#
KaratsubaPolynomialMultiplicationExtRep:=function( f, g )
local degf, degg, deg, n, halfn, nr, f1, f0, g1, g0, u, v, w, wuv, k, pos, pos1, val;
# Zero polynomial will be represented as [ [  ], 0 ]
# We took care that other representations of zero, for example [ [ 0, 0 ], 0 ] 
# or [ [ 0, 0 ], 1 ], or [ [ 0 ], 1 ], can not occure, because we reduce
# the presentation after adding polynomials in PlusLaurentPolynomialsExtRep
# and subtracting them in MinusLaurentPolynomialsExtRep. Note that degree
# determination is also based on this feature.
if f[1]=[] or g[1]=[] then
  return [ [ ], 0 ];
fi;
if Length(f[1]) < KARATSUBA_CUTOFF and Length(g[1]) < KARATSUBA_CUTOFF then
  return [ PRODUCT_COEFFS_GENERIC_LISTS( f[1], Length(f[1]), g[1], Length(g[1]) ), f[2]+g[2] ];
fi;
# We determine degree from valuation and length of the coefficients list
degf := f[2]+Length(f[1])-1;
degg := g[2]+Length(g[1])-1;
deg := Maximum( degf, degg );
n:=1;
while n < deg do
  n:=n*2;
od;
# we can proceed immediately, since the case n=1 already caught by KARATSUBA_CUTOFF
halfn := n/2;
# developing the 1st polynomial
if degf >= halfn then
  k:=halfn-f[2]+1;
  if k<1 then
    pos:=1;
    val:=1-k;
  else
    pos:=k; 
    val:=0; 
  fi;  
  # we remove initial zeroes in the quotient, if such exist
  pos1 := First([ pos .. Length(f[1])], k -> f[1][k] <> 0 );
  f1 := [ f[1]{[ pos1 .. Length(f[1])]}, val+pos1-pos ]; # EuclideanQuotient( f, b )   
  # we remove trailing zeroes in the remainder, if such exist
  pos1 := First( [ 1 .. halfn-f[2] ], k -> f[1][halfn-f[2]-k+1] <> 0 ); 
  f0 := [ f[1]{[ 1 .. halfn-f[2]-pos1+1 ]}, f[2] ];  # EuclideanRemainder( f, b )
else
  f1 := [ [ ], 0 ];
  f0 := f;
fi;
# developing the 2nd polynomial
if degg >= halfn then
  k:=halfn-g[2]+1;
  if k<1 then
    pos:=1;
    val:=1-k;
  else
    pos:=k; 
    val:=0; 
  fi; 
  # we remove initial zeroes in the quotient, if such exist
  pos1 := First([ pos .. Length(g[1])], k -> g[1][k] <> 0 );
  g1 := [ g[1]{[ pos1 .. Length(g[1])]}, val+pos1-pos ]; # EuclideanQuotient( g, b )    
  # we remove trailing zeroes in the remainder, if such exist
  pos1 := First( [ 1 .. halfn-g[2] ], k -> g[1][halfn-g[2]-k+1] <> 0 ); 
  g0 := [ g[1]{[ 1 .. halfn-g[2]-pos1+1 ]}, g[2] ];  # EuclideanRemainder( g, b )    
else
  g1 := [ [ ], 0 ];
  g0 := g;
fi;  
# three recursive calls
u := KaratsubaPolynomialMultiplicationExtRep(f1,g1);
v := KaratsubaPolynomialMultiplicationExtRep(f0,g0);
w := KaratsubaPolynomialMultiplicationExtRep(
       PlusLaurentPolynomialsExtRep(f1,f0),
       PlusLaurentPolynomialsExtRep(g1,g0) );
# composing the result
wuv :=  MinusLaurentPolynomialsExtRep( MinusLaurentPolynomialsExtRep(w,u), v );
wuv[2] := wuv[2] + halfn;
u[2] := u[2] + n;
return PlusLaurentPolynomialsExtRep( PlusLaurentPolynomialsExtRep(u,wuv), v );
# return u*(b^2) + (w-u-v)*b + v;
end;


#############################################################################
#
# This is the top-level function that accepts polynomials in the natural
# presentation. The actual job is done by the recursively called function 
# KaratsubaPolynomialMultiplicationExtRep. Nevertheless, we perform the 
# first step in the top-level function instead of just passing the lists
# of coefficients to the KaratsubaPolynomialMultiplicationExtRep. This 
# allows us to shorten the size of input, that maybe especially essential
# if KaratsubaPolynomialMultiplicationExtRep will be called as a web service.
#
KaratsubaPolynomialMultiplication:=function( f, g )
local deg, n, halfn, x, b, nr, fam, f1, f0, g1, g0, u, v, w, wuv,
      cf, cg, k, pos, pos1, val, result;
if IsZero(f) or IsZero(g) then 
  return Zero(f);
fi;
deg := Maximum( List( [f,g], DegreeOfLaurentPolynomial ) );
n:=1;
while n < deg do
  n:=n*2;
od;
if n=1 then
  return f*g;
else
  halfn := n/2;
  x := IndeterminateOfUnivariateRationalFunction( f );
  nr := IndeterminateNumberOfLaurentPolynomial(f); 
  fam := CoefficientsFamily( FamilyObj( f ) );
  # developing the 1st polynomial
  if DegreeOfLaurentPolynomial(f) >= halfn then
    cf := CoefficientsOfLaurentPolynomial( f );
    k:=halfn-cf[2]+1;
    if k<1 then
      pos:=1;
      val:=1-k;
    else
      pos:=k; 
      val:=0; 
    fi;  
    # we remove initial zeroes in the quotient, if such exist
    pos1 := First([ pos .. Length(cf[1])], k -> cf[1][k] <> 0 );
    f1 := [ cf[1]{[ pos1 .. Length(cf[1])]}, val+pos1-pos ]; # EuclideanQuotient( f, b )    
    # we remove trailing zeroes in the remainder, if such exist
    pos1 := First( [ 1 .. halfn-cf[2] ], k -> cf[1][halfn-cf[2]-k+1] <> 0 ); 
    f0 := [ cf[1]{[ 1 .. halfn-cf[2]-pos1+1 ]}, cf[2] ];  # EuclideanRemainder( f, b )
  else
    f1 := [ [ ], 0 ];
    f0 := CoefficientsOfLaurentPolynomial( f );
  fi;
  # developing the 2nd polynomial
  if DegreeOfLaurentPolynomial(g) >= halfn then
    cg := CoefficientsOfLaurentPolynomial( g );
    k:=halfn-cg[2]+1;
    if k<1 then
      pos:=1;
      val:=1-k;
    else
      pos:=k; 
      val:=0; 
    fi; 
    # we remove initial zeroes in the quotient, if such exist
    pos1 := First([ pos .. Length(cg[1])], k -> cg[1][k] <> 0 );
    g1 := [ cg[1]{[ pos1 .. Length(cg[1])]}, val+pos1-pos ]; # EuclideanQuotient( g, b )    
    # we remove trailing zeroes in the remainder, if such exist
    pos1 := First( [ 1 .. halfn-cg[2] ], k -> cg[1][halfn-cg[2]-k+1] <> 0 ); 
    g0 := [ cg[1]{[ 1 .. halfn-cg[2]-pos1+1 ]}, cg[2] ];  # EuclideanRemainder( g, b )    
  else
    g1 := [ [ ], 0 ];
    g0 := CoefficientsOfLaurentPolynomial( g );
  fi;  
  # three recursive calls
  u := KaratsubaPolynomialMultiplicationExtRep(f1,g1);
  v := KaratsubaPolynomialMultiplicationExtRep(f0,g0);
  w := KaratsubaPolynomialMultiplicationExtRep(
         PlusLaurentPolynomialsExtRep(f1,f0),
         PlusLaurentPolynomialsExtRep(g1,g0) );
  # composing the result        
  wuv :=  MinusLaurentPolynomialsExtRep( MinusLaurentPolynomialsExtRep(w,u), v );
  wuv[2] := wuv[2] + halfn;
  u[2] := u[2] + n;  
  result := PlusLaurentPolynomialsExtRep( PlusLaurentPolynomialsExtRep(u,wuv), v );
  return LaurentPolynomialByCoefficients( fam, result[1], result[2], nr );
  # return u*(b^2) + (w-u-v)*b + v;  
fi; 
end;


#############################################################################
#
# This is the web-service using version of KaratsubaPolynomialMultiplication
#
KaratsubaPolynomialMultiplicationWS:=function( f, g )
local deg, n, halfn, x, b, nr, fam, f1, f0, g1, g0, u, v, w, wuv,
      cf, cg, k, pos, pos1, val, wsresult, result;
if IsZero(f) or IsZero(g) then 
  return Zero(f);
fi;
deg := Maximum( List( [f,g], DegreeOfLaurentPolynomial ) );
n:=1;
while n < deg do
  n:=n*2;
od;
if n=1 then
  return f*g;
else
  halfn := n/2;
  x := IndeterminateOfUnivariateRationalFunction( f );
  nr := IndeterminateNumberOfLaurentPolynomial(f); 
  fam := CoefficientsFamily( FamilyObj( f ) );
  # developing the 1st polynomial
  if DegreeOfLaurentPolynomial(f) >= halfn then
    cf := CoefficientsOfLaurentPolynomial( f );
    k:=halfn-cf[2]+1;
    if k<1 then
      pos:=1;
      val:=1-k;
    else
      pos:=k; 
      val:=0; 
    fi;  
    # we remove initial zeroes in the quotient, if such exist
    pos1 := First([ pos .. Length(cf[1])], k -> cf[1][k] <> 0 );
    f1 := [ cf[1]{[ pos1 .. Length(cf[1])]}, val+pos1-pos ]; # EuclideanQuotient( f, b )
    # we remove trailing zeroes in the remainder, if such exist
    pos1 := First( [ 1 .. halfn-cf[2] ], k -> cf[1][halfn-cf[2]-k+1] <> 0 ); 
    f0 := [ cf[1]{[ 1 .. halfn-cf[2]-pos1+1 ]}, cf[2] ];  # EuclideanRemainder( f, b )
  else
    f1 := [ [ ], 0 ];
    f0 := CoefficientsOfLaurentPolynomial( f );
  fi;
  # developing the 2nd polynomial
  if DegreeOfLaurentPolynomial(g) >= halfn then
    cg := CoefficientsOfLaurentPolynomial( g );
    k:=halfn-cg[2]+1;
    if k<1 then
      pos:=1;
      val:=1-k;
    else
      pos:=k; 
      val:=0; 
    fi; 
    # we remove initial zeroes in the quotient, if such exist
    pos1 := First([ pos .. Length(cg[1])], k -> cg[1][k] <> 0 );
    g1 := [ cg[1]{[ pos1 .. Length(cg[1])]}, val+pos1-pos ]; # EuclideanQuotient( g, b )    
    # we remove trailing zeroes in the remainder, if such exist
    pos1 := First( [ 1 .. halfn-cg[2] ], k -> cg[1][halfn-cg[2]-k+1] <> 0 ); 
    g0 := [ cg[1]{[ 1 .. halfn-cg[2]-pos1+1 ]}, cg[2] ];  # EuclideanRemainder( g, b )    
  else
    g1 := [ [ ], 0 ];
    g0 := CoefficientsOfLaurentPolynomial( g );
  fi;  
  # three recursive calls
  # u := KaratsubaPolynomialMultiplicationExtRep(f1,g1);
  # v := KaratsubaPolynomialMultiplicationExtRep(f0,g0);

  u := NewProcess( "WS_Karatsuba",[ String(f1), String(g1) ],"localhost", 26133);   
  v := NewProcess( "WS_Karatsuba",[ String(f0), String(g0) ],"localhost", 26134);   
  w := KaratsubaPolynomialMultiplicationExtRep(
         PlusLaurentPolynomialsExtRep(f1,f0),
         PlusLaurentPolynomialsExtRep(g1,g0) );
         
  wsresult:=SynchronizeProcesses2( u,v );
  u := EvalString( wsresult[1].object );
  v := EvalString( wsresult[2].object );
     
  # composing the result        
  wuv :=  MinusLaurentPolynomialsExtRep( MinusLaurentPolynomialsExtRep(w,u), v );
  wuv[2] := wuv[2] + halfn;
  u[2] := u[2] + n;  
  result := PlusLaurentPolynomialsExtRep( PlusLaurentPolynomialsExtRep(u,wuv), v );
  return LaurentPolynomialByCoefficients( fam, result[1], result[2], nr );
  # return u*(b^2) + (w-u-v)*b + v;  
fi; 
end;