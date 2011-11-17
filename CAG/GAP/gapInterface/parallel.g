Print("\nCAG Interface by A. Al Zain <ceeatia.macs.hw.ac.uk> \n\n");
########################################################################################################
parseStr := function(results)
        local finalResults, i, j, resultLength, tmp, tmpResults;
    j := 1;
    Print("parseStr, input Result => ", results, "\n");
    finalResults:=[];
    tmp:="";
    tmpResults := results;
    Remove(tmpResults,1);
    Print("tmpResults ..", tmpResults, "\n");
    resultLength:= Length(tmpResults);
    Remove(tmpResults,resultLength);;
    resultLength:= Length(tmpResults);
    Remove(tmpResults,resultLength);;
    for i in results do
        if i = '\"' then
            continue;
        elif i = ',' then
            finalResults[j]:=tmp;
            tmp:= "";
           j:= j+1;
        else
            Append(tmp,[i]);
        fi;
    od;
    finalResults[j]:=tmp ;
    return finalResults;

end;;
################################### RunHaskellProgram ###################################
RunHaskellProgram := function ( arg )
    local  cmd, i, shell, dir,s;
    cmd := ShallowCopy( arg[1] );
    if not IsString( cmd )  then
        Error( "the command ", cmd, " is not a name.\n", "possibly a binary is missing or has not been compiled." );
    fi;
    for i  in [ 2 .. Length( arg ) ]  do
        Append( cmd, " " );
        Append( cmd, arg[i] );
    od;
    s := " ";
    shell := Filename( DirectoriesSystemPrograms(  ), "sh" );
    dir := DirectoryCurrent(  );
    Process( dir, shell, InputTextUser(  ), OutputTextString( s, false ), [ "-c", cmd ] );
    return s;
end;;

_EVALSTRINGTMP := 0;
dist_EvalString := function ( s )
    local  a, f, res;
    a := "_EVALSTRINGTMP:=";
    Append( a, s );
    Add( a, ';' );
    Unbind( _EVALSTRINGTMP );
    f := InputTextString( a );
    Read( f );
    if not IsBound( _EVALSTRINGTMP )  then
        res := s;
    else
        res := _EVALSTRINGTMP;
    fi;
    Unbind( _EVALSTRINGTMP );
    return res;
end;

########################################################################################################
#################################### GAP  ##############################################################
################################### parMap  ###################################
GAPparMap := function(funName, myList)
    local listLength,i, runCommand, myArgs, finalResults;
    listLength :=  Length(myList);
    funName := Concatenation("\"",funName,"\"");
    myArgs := "";
    for i in myList do
        Append( myArgs, Concatenation("\"",String(i),"\" ") );
    od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/GAP/gapSkeleton/parMap/parMap ";
#    Print(runCommand,funName, " ",myArgs," +RTS -qN", String(listLength));
    myArgs := Concatenation(runCommand,funName, " ",myArgs," +RTS -qN", String(listLength));   
    finalResults := RunHaskellProgram(myArgs);
    return EvalString(finalResults);
end;;
Print("******** parallel map in GAP (GAPparMap) is loaded ...\n");

################################### masterWorkers ###################################
GAPmasterWorkers := function(funName, chunk, slaves, myList)
    local listLength, i, runCommand, myArgs, finalResults, sugSlaves;
    listLength :=  Length(myList);
    sugSlaves := Int(listLength/chunk);
    if slaves > sugSlaves then slaves := sugSlaves; fi;
    myArgs := "";
    funName := Concatenation("\"",funName,"\"");
    for i in myList do
        Append( myArgs, Concatenation("\"",String(i),"\" ") );
    od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/GAP/gapSkeleton/masterWorkers/masterWorkers ";
    myArgs := Concatenation(runCommand,funName, " ",String(chunk) ," ",myArgs," +RTS -qN", String(slaves), " -qQ83875840");   
    finalResults := RunHaskellProgram(myArgs);
    return EvalString(finalResults);

end;;
Print("******** master-Workers in GAP (GAPmasterWorkers) is loaded ...\n"); 

################################### farm ###################################
GAPfarm := function(funName, pes, myList)
    local listLength, i, runCommand, myArgs, finalResults;
    listLength :=  Length(myList);
    myArgs := "";
    funName := Concatenation("\"",funName,"\"");
    for i in myList do
        Append( myArgs, Concatenation("\"",String(i),"\" ") );
    od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/GAP/gapSkeleton/farm/farm ";
#    Print(runCommand,funName, " ",myArgs," +RTS -qN", String(pes));
    myArgs := Concatenation(runCommand,funName ," ",myArgs," +RTS -qN", String(pes), " -qQ8387584");   
    finalResults := RunHaskellProgram(myArgs);
    return EvalString(finalResults);
end;;
Print("******** parallel farm in GAP (GAPfarm) is loaded ...\n"); 

################################### parZipWith ###################################
GAPparZipWith := function(funName, pes, list1, list2)
    local length1, length2, i, runCommand, myArgs, finalResults;
    length1 := Length(list1);
    length2 := Length(list2);
#    list1 := List(list1,String);
#    list2 := List(list2,String);

    funName := Concatenation("\"",funName,"\"");
    myArgs := "";
    for i in list1 do Append( myArgs, Concatenation("\"",String(i),"\" ") ); od;
 
#   for i in list1 do
#        myArgs:= Concatenation(myArgs,"\"",i,"\" ");
#    od;

    for i in list2 do  Append( myArgs, Concatenation("\"",String(i),"\" ") );  od;

#    for i in list2 do
#        myArgs:= Concatenation(myArgs,"\"",i,"\" ");
#    od;

    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/GAP/gapSkeleton/parZipWith/parZipWith ";
#    Print(runCommand,funName, " ",String(length1), " ",myArgs," +RTS -qN", String(pes));
    myArgs := Concatenation(runCommand,funName ," ",String(length1), " ",myArgs," +RTS -qN", String(pes), " -qQ8387584");   
#    Print(myArgs, " \n");
    finalResults := RunHaskellProgram(myArgs);
    return EvalString(finalResults);
end;;
Print("******** parallel zipWith in GAP (GAPparZipWith) is loaded ...\n"); 

################################### parReduce  ###################################
GAPparReduce := function(funName, pes, myList)
    local listLength, i, runCommand, myArgs, finalResults;
    listLength :=  Length(myList);
#    myList := List(myList, String);
#Print("myList=>",myList,"\n");
    funName := Concatenation("\"",funName,"\"");
    myArgs := "";
    for i in myList do Append( myArgs, Concatenation("\"",String(i),"\" ") ); od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/GAP/gapSkeleton/parReduce/parReduce ";
    myArgs := Concatenation(runCommand,funName ," ",myArgs," +RTS -qN", String(pes), " -qQ8387584");   
    finalResults := RunHaskellProgram(myArgs);
#    Print("finalResults=>", finalResults, "\n");
    return EvalString(finalResults);
end;;
Print("******** parallel Reduce (parReduce) is loaded ...\n"); 


####################################  Maple ############################################################
################################### mapleParMap ###################################
mapleParMap := function(funName, myList)
    local listLength, i, runCommand, myArgs, finalResults;
    listLength :=  Length(myList);
    myArgs := "";
    for i in myList do
        myArgs:= Concatenation(myArgs,"\"",String(i),"\" ");
    od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/Maple/mapleSkeleton/parMap/parMap ";
#    Print(runCommand,funName, " ",myArgs," +RTS -qN", String(listLength));
    myArgs := Concatenation(runCommand,funName, " ",myArgs," +RTS -qN", String(listLength));   
#    Print(myArgs);
    finalResults := RunHaskellProgram(myArgs);
#    finalResults := List(finalResults,OMString);    
    Print (finalResults);
end;;

Print("******** parallel Map in Mapel (mapleParMap) is loaded ...\n");
########################################################################################################
#################################### MuPAD  ##############################################################
################################### parMap  ###################################
MuPADparMap := function(funName, myList)
    local listLength,i, runCommand, myArgs, finalResults;
    listLength :=  Length(myList);
    funName := Concatenation("\"",funName,"\"");
    myArgs := "";
    for i in myList do
        Append( myArgs, Concatenation("\"",String(i),"\" ") );
    od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/MuPAD/mupadSkeleton/parMap/parMapMuPAD ";
#    Print(runCommand,funName, " ",myArgs," +RTS -qN", String(listLength));
    myArgs := Concatenation(runCommand,funName, " ",myArgs," +RTS -qN", String(listLength));   
    finalResults := RunHaskellProgram(myArgs);
    return EvalString(finalResults);
end;;
Print("******** parallel Map in MuPAD (MuPADparMap) is loaded ...\n");
################################### masterWorkers ###################################
MuPADmasterWorkers := function(funName, chunk, slaves, myList)
    local listLength, i, runCommand, myArgs, finalResults, sugSlaves;
    listLength :=  Length(myList);
    sugSlaves := Int(listLength/chunk);
    if slaves > sugSlaves then slaves := sugSlaves; fi;
    #myList := List(myList,String);
    myArgs := "";
    funName := Concatenation("\"",funName,"\"");
    for i in myList do
        Append( myArgs, Concatenation("\"",String(i),"\" ") );
    od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/MuPAD/mupadSkeleton/masterWorkers/masterWorkersMuPAD ";
    myArgs := Concatenation(runCommand,funName, " ",String(chunk) ," ",myArgs," +RTS -qN", String(slaves), " -qQ8387584");   
    finalResults := RunHaskellProgram(myArgs);
    return EvalString(finalResults);
end;;
Print("******** masterWorkers in MuPAD (MuPADmasterWorkers) is loaded ...\n");
################################### farm ###################################
MuPADfarm := function(funName, pes, myList)
    local listLength, i, runCommand, myArgs, finalResults;
    listLength :=  Length(myList);
    myArgs := "";
    funName := Concatenation("\"",funName,"\"");
    for i in myList do
        Append( myArgs, Concatenation("\"",String(i),"\" ") );
    od;
    runCommand := "/u1/staff/ceeatia/GHC-GAP-0.2/CAG/MuPAD/mupadSkeleton/farm/farmMuPAD ";
#    Print(runCommand,funName, " ",myArgs," +RTS -qN", String(pes));
    myArgs := Concatenation(runCommand,funName ," ",myArgs," +RTS -qN", String(pes), " -qQ8387584");   
    finalResults := RunHaskellProgram(myArgs);
    return EvalString(finalResults);
end;;
Print("******** parallel farm in MuPAD (MuPADfarm) is loaded ...\n");
####################################################################################
############################HELP function ##########################################
helpParMap := function()
   Print("Help: Showing reference for parMap\n\n",
        "\t parMap(\"FunctionName\", [<elements>])\n\n",
        "parMap applies \"FunctionName\" to each element in the list and\n",
        "returns a list of output [<element>]\n",
        "from a parallel point of view:\n",
        "parMap generate a parallel task for each element in the list\n\n" ); 
end;;
Print("  \t more information how to use parMap call helpParMap();\n");


helpMasterWorkers := function()
   Print("Help: Showing reference for masterWorkers\n\n",
        "\t masterWorkers (\"FunctionName\", chunk-Size, no-of-workers, [<elements>])\n\n",
        "masterWorkers chunks the list according to the chunk-size.\n",
        "It splits part of the jobs between workers, and the rest remains on the master.\n",
        "It resturns the result of applying \"FunctionName\" to the elements of the list\n"); 
   
end;;
Print(" \t more information how to use masterWorkers call helpMasterWorkers();\n");


helpFarm := function()
   Print("Help: Showing reference for farm\n\n",
        "\t farm (\"FunctionName\",no-pes,[<elements>])\n\n",
        "farm is a smart implementation of parMap.\n",
        "farm applies \"FunctionName\" to each element in the list and\n",
        "returns a list of output [<element>]\n",
        "from a parallel point of view:\n",
        "farm generate enough parallel tasks to accommodate available PEs.\n\n" ); 
end;;
Print(" \t more information how to use farm call helpFarm();\n");


helpParZipWith := function()
   Print("Help: Showing reference for parZipWith\n\n",
        "\t parZipWith (\"FunctionName\", no-pes, [<elements>], [<elements>])\n\n",
        "example: parZipWith(\"Gcd\", 3, [100..110],[200..210]) \n",
        "parZipWith generates a single list results from applying \"FunctionName\" for both lists.\n\n" ); 
end;;
Print(" \t more information how to use parZipWith call helpParZipWith();\n");



helpParReduce := function()
   Print("Help: Showing reference for parReduce\n\n",
        "\t parReduce (\"FunctionName\",no-pes,[<elements>])\n\n",
        "parReduce applies \"FunctionName\" to the List to\n",
        "returns a single ouput\n",
        "from a parallel point of view:\n",
        "parReduce generates subLists from the main list to be evaluated in parallel according to the number of PEs.\n\n" ); 
end;;
Print(" \t more information how to use parReduce call helpParReduce();\n");
