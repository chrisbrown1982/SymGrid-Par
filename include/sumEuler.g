# Time-stamp: <Wed Jun 23 2010 21:32:17 Stardate: Stardate: [-28]3269.27 hwloidl>
#
# This version is used in SymGrid-Par v0.3 to run some simple tests
# -----------------------------------------------------------------------------

hcf_recursive:=function(x,y)
local m;
if y = 0 then 
  return x;
else
  m := x mod y;
  return hcf_recursive(y,m);  
fi;
end;  

hcf_nonrecursive:=function(x,y)
local m;
m := x mod y;
while m <> 0 do
  x:=y;  y:=m;  m := x mod y;od;return y;
end;  

#############################################################################

relprime_recursive := function(x,y)
local m;
m:=hcf_recursive(x,y);
return m=1;
end;

relprime_nonrecursive := function(x,y)
local m;
m:=hcf_nonrecursive(x,y);
return m=1;
end;

relprime_classic := function(x,y)
local m;
m:=GcdInt(x,y);
return m=1;
end;

#############################################################################

euler_recursive := function(n)
local x;
x := Number(Filtered( [ 1..n ], x -> relprime_recursive(x,n) ) );
return x;
end;

euler_nonrecursive := function(n)
local x;
x := Number(Filtered( [ 1..n ], x -> relprime_nonrecursive(x,n) ) );
return x;
end;

euler_classic := function(n)
local x;
x := Number(Filtered( [ 1..n ], x -> relprime_classic(x,n) ) );
return x;
end;

#############################################################################

sumEuler_recursive:=function(n,m)
local result, x;
result:=Sum( [ n..m ], x -> euler_recursive(x) ); 
return result;
end;

sumEuler_nonrecursive:=function(n,m)
local result, x;
result:=Sum( [ n..m ], x -> euler_nonrecursive(x) ); 
return result;
end;

sumEuler_classic:=function(n,m)
local result, x;
result:=Sum( [ n..m ], x -> euler_classic(x) ); 
return result;
end;

SumEulerRange:=function(n,m)
local result, x;
result:=Sum( [ n..m ], x -> euler_classic(x) ); 
return result;
end;
