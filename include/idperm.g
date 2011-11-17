# $Id: idperm.g 2846 2009-04-18 23:25:47Z alexk $

IdGroupWS := function( G )
# this is the client's counterpart for the SCSCP service for the 
# indentification of groups in the GAP Small Groups Library.
# (See the procedure GroupIdentificationService in scscp/example/myserver.g)
# The group is converted to the permutation group and then its generators
# will be sent to the server for the identification.
local H, result;
if not IsPermGroup(G) then
  H:= Image( IsomorphismPermGroup( G ) );
else
  H := G;
fi;  
result := EvaluateBySCSCP ( "GroupIdentificationService", 
                                [ GeneratorsOfGroup(H) ], 
                                "localhost", 26133 );
return result.object;
end;

TestIdGroupWS:=function( nrcalls )
# This is the test to generate a sequence of calls to the
# previous function, the length of this sequence is the
# input parameter nrcalls of the test
local k, s, n, G;
for k in [1..nrcalls] do
s := Random( [ 1 .. 1000 ] );
n := Random( [ 1 ..NrSmallGroups(s)]);
G := SmallGroup( s, n );
Print( "Sending  ", IdGroup( G ), " ... \n" );
Print( "Got back ", IdGroupWS( G ), "\n" );
od;
end;