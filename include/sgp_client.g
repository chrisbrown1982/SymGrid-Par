# 
# -----------------------------------------------------------------------------
# basic packages needed

LoadPackage("scscp");
Read("sumEuler.g");

SetInfoLevel(InfoSCSCP,0);

# -----------------------------------------------------------------------------
# global constants

SCSCPclientHost := "localhost";
SCSCPclientPort := 12321;

# -----------------------------------------------------------------------------
# some worker functions

QuillenSeriesByIdGroup := function( id )
local G, qs, latt, msl, ccs, ccs_repr, i, x, n;
G := SmallGroup( id );
latt := LatticeSubgroups(G);
msl := MinimalSupergroupsLattice(latt);
ccs := ConjugacyClassesSubgroups(latt);
ccs_repr := List(ccs, Representative);
qs := [];
for i in [ 1 .. LogInt( Size(G), PrimePGroup(G) ) ] do
  qs[i]:=0;
od;
for i in [ 1 .. Length(ccs_repr) ] do 
  if IsElementaryAbelian( ccs_repr[i] ) then
    if ForAll( msl[i], 
               x -> IsElementaryAbelian( ccs[x[1]][x[2]] ) = false ) then
      n := LogInt( Size(ccs_repr[i]), PrimePGroup(G) );
      qs[n] := qs[n] + 1;
    fi;
  fi;
od;
return [ id, qs ];
end;


PointImages:=function( G, n )
local g;
return Set( List( GeneratorsOfGroup(G), g -> n^g ) );
end;

# -----------------------------------------------------------------------------
# Useful shortcuts

RandomPolynomial:=function(n)
local fam, f;    
fam:=FamilyObj(1);
f:=LaurentPolynomialByCoefficients( fam, List([1..n],i->Random(Integers)), 0, 1 );
return f;
end;

RandomPolynomialAsString:=function(n)
return String(RandomPolynomial(n));
end;

Run1:=function( fct, args )
local res;    
res:=EvaluateBySCSCP("CS_parProcesses1", [fct, args], SCSCPclientHost, SCSCPclientPort);
return res.object;
end;
    
Run2:=function( fct1, fct2, args1, args2 )
local res;    
res:=EvaluateBySCSCP("CS_parProcesses2", [fct1, fct2, args1, args2], SCSCPclientHost, SCSCPclientPort);
return res.object;
end;
    
ParMap:=function( map_fct, list )
local res;
res:=EvaluateBySCSCP("CS_parMap", [ map_fct, list ], SCSCPclientHost, SCSCPclientPort);
return res.object;
end;

ParMapFold:=function( map_fct, fold_fct, neutral, list )
local res;
res:=EvaluateBySCSCP("CS_parMapFold", [ map_fct, fold_fct, neutral, list ], SCSCPclientHost, SCSCPclientPort);
return res.object;
end;

ParMapFold1:=function( map_fct, fold_fct, neutral, list )
local res;
res:=EvaluateBySCSCP("CS_parMapFold1", [ map_fct, fold_fct, neutral, list ], SCSCPclientHost, SCSCPclientPort);
return res.object;
end;

ParZipWith:=function( zip_fct, list1, list2 )
local res;
res:=EvaluateBySCSCP("CS_parZipWith", [ zip_fct, list1, list2 ], SCSCPclientHost, SCSCPclientPort);
return res.object;
end;

ParOrbit:=function( gen_fct, start_list, set_size )
local res;
res:=EvaluateBySCSCP("CS_Orbit", [gen_fct, start_list, set_size], SCSCPclientHost, SCSCPclientPort);
return res.object;
end;

F:=GF(3);
l:=AsList(F);

# -----------------------------------------------------------------------------

ys:=[10,9,8,7,6,5,4,3,2,1]; 
xs:=[1,2,3,4,5,6,7,8,9,10];
zs:=[87,88,89];
     
fam:=FamilyObj(1);
n:=20;
x_1:=Indeterminate(Rationals,"x_1");
x_2:=Indeterminate(Rationals,"x_2");
x_3:=Indeterminate(Rationals,"x_3");
x:=Indeterminate(Rationals,"x");
y:=Indeterminate(Rationals,"y");
z:=Indeterminate(Rationals,"z");

# ./testClient 12321 karaPar0 20
p1:=RandomPolynomial(n);
p2:=RandomPolynomial(n);
p3:=RandomPolynomial(n);
p4:=RandomPolynomial(n);
s1:=String(p1);
s2:=String(p2);
s3:=String(p3);
s4:=String(p4);
# EvaluateBySCSCP("CS_karatsubaParSCSCP", [s1,s2,s3,s4], SCSCPclientHost, SCSCPclientPort);

# GroebnerBasis (experimental)
p01:=x_1^2+x_2^2+x_3^2-1;
p02:=x_1^2+x_3^2-x_2;
p03:=x_1-x_2;
s01:=String(p01);
s02:=String(p02);
s03:=String(p03);
#  EvaluateBySCSCP("CS_GroebnerBasis", [3, [s01,s02,s03]], SCSCPclientHost, SCSCPclientPort);
# l00:=[p01,p02,p03];
# GroebnerBasis(l00,MonomialLexOrdering());

q1:=RandomPolynomial(3);
q2:=RandomPolynomial(3);
q3:=RandomPolynomial(3);
qs1:=String(q1);
qs2:=String(q2);
qs3:=String(q3);
#  EvaluateBySCSCP("CS_GroebnerBasis", [3, [qs1,qs2,qs3]], SCSCPclientHost, SCSCPclientPort);
