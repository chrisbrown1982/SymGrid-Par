# $Id: id512.g 2846 2009-04-18 23:25:47Z alexk $

IdGroup512:=function( G )
# this is the client's counterpart for the SCSCP service for the 
# identification of groups of order 512 using the ANUPQ package. 
# (See the procedure IdGroup512ByCode in scscp/example/myserver.g).
# The group must have PC presentation and its pcgs code will be 
# sent to the server to identify the group.
local code, result;
if Size( G ) <> 512 then
  Error( "G must be a group of order 512 !!!\n" );
fi;
code := CodePcGroup( G );
result := EvaluateBySCSCP( "IdGroup512ByCode", [ code ], 
                           "localhost", 26133 );
return result.object;
end;

IdGroup512:=function( G )
# this is the client's counterpart for the SCSCP service for the 
# identification of groups of order 512 using the ANUPQ package. 
# (See the procedure IdGroup512ByCode in scscp/example/myserver.g).
# The group must have PC presentation and its pcgs code will be 
# sent to the server to identify the group.
local code, result;
if Size( G ) <> 512 then
  Error( "G must be a group of order 512 !!!\n" );
fi;
code := CodePcGroup( G );
result := EvaluateBySCSCP( "IdGroup512ByCode", [ code ], 
                           "localhost", 26133 );
                           # "chrystal.mcs.st-and.ac.uk", 26133 );
return result.object;
end;


TestIdGroup512:=function( nrcalls )
# This is the test to generate a sequence of calls to the
# previous function, the length of this sequence is the
# input parameter nrcalls of the test
local numbers, n, G;
for n in [ 1 .. nrcalls ] do
  G := SmallGroup( 512, n );
  Print( "Sending  ", IdGroup( G ), " ... \n" );
  Print( "Got back ", IdGroup512( G ), "\n" );
od;
end;