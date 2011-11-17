# Time-stamp: <Thu Jun 10 2010 22:08:10 Stardate: Stardate: [-28]3204.40 hwloidl>
#############################################################################
# Demo of using SymGrid-Par from within a GAP shell
#
# Setup (see USE document of the SymGrid-Par distribution):
# . launch several GAP servers from the commandline, eg.
#   gap.sh sgp_server.g
# . launch the coordination server from the command line
#   ./CoordinationServer_pp --verbose --debug -C 12321 +RTS -qp2
# . launch a gap.sh and run this script, step-by-step
#   gap.sh
#############################################################################

# -----------------------------------------------------------------------------
# init
LoadPackage("scscp");
Read("sumEuler.g");
# -----------------------------------------------------------------------------
# connect to GAP server
EvaluateBySCSCP("WS_Phi", [87], "localhost", 26154);
EvaluateBySCSCP("WS_Fibonacci", [13], "localhost", 26154);
# -----------------------------------------------------------------------------
# connect to Coordination Server
EvaluateBySCSCP("CS_Phi", [87], "localhost", 12321);
# ./testClient 12321 fib 11
EvaluateBySCSCP("CS_Fib", [11], "localhost", 12321);
# standard test launching 2 instances of sumEuler
# ./testClient 12321 sumEulerClassic 13000 11000
EvaluateBySCSCP("CS_sumEulerClassicSCSCP", [13000, 11000], "localhost", 12321);
# ISSAC demo: std sumEuler example
# ./testClient 12321 sumEulerPar 8000 2000
EvaluateBySCSCP("CS_sumEulerPar", [8000, 2000], "localhost", 12321);
# different name for the same thing
EvaluateBySCSCP("CS_SumEuler", [8000, 2000], "localhost", 12321);
# ./testClient 12321 parMapFold WS_Phi WS_Plus 0 87 88 89
EvaluateBySCSCP("CS_parMapFold", ["WS_Phi", "WS_Plus", 0, [87, 88, 89]], "localhost", 12321);
# ./testClient 12321 parMapFold1 WS_Phi WS_Plus 87 88 89
EvaluateBySCSCP("CS_parMapFold1", ["WS_Phi", "WS_Plus", [87, 88, 89]], "localhost", 12321);
# ./testClient 12321 parMapFold WS_Res WS_Plus 0 92 93 94
EvaluateBySCSCP("CS_parMapFold", ["WS_Res", "WS_Plus", 0, [92, 93, 94]], "localhost", 12321);

# -----------------------------------------------------------------------------
# LOG
# --(ladybank02[46](3.2))-- gap.sh sgp_server.g 
# Edit config.g to:  SCSCPserverPort := 26154;
# --(ladybank02[46](3.2))-- gap.sh sgp_server.g 
# --(ladybank02[56](3.2))-- ./CoordinationServer_pp --verbose --debug -C 12321 +RTS -qp2
# --(ladybank02[57](3.2))-- cd /scratch/hwloidl/txvi/SGP_v0.3.1/lib
# --(ladybank02[58](3.2))-- gap.sh
# gap> 
# gap> LoadPackage("scscp");
# gap> Read("sumEuler.g");
# gap> EvaluateBySCSCP("Fib", [11], "localhost", 12321);
# #I  Got back: object 89 with attributes 
# [ [ "call_id", "coordinationServer:12321:6Imr75R8" ] ]
# rec( object := 89, 
#   attributes := [ [ "call_id", "coordinationServer:12321:6Imr75R8" ] ] )
# gap> EvaluateBySCSCP("sumEulerPar", [8000, 2000], "localhost", 12321);
# #I  Got back: object 19455782 with attributes 
# [ [ "call_id", "coordinationServer:12321:34xhmCty" ] ]
# rec( object := 19455782, 
#   attributes := [ [ "call_id", "coordinationServer:12321:34xhmCty" ] ] )
