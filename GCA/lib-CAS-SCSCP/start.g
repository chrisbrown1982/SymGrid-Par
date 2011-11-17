# -----------------------------------------------------------------------------
# Haskell-GAP interface
# 
# (c) 2006, A. D. I. Al Zain <CEEATIA@MACS.HW.AC.UK>
# http://www.macs.hw.ac.uk/~ceeatia
# 
# $Id: start.g,v 1.23 2006/09/08 08:38:40 gapchron Exp $
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# server protocol
#
# 0. send "ready"
# 1. receive name of file for error messages
# 2. sequence of "receive -> send" pairs
#
# in  text               -> m exp         gap exp from external format
# out exp                -> m text        external format from gap exp
# fun n text exp1...expn -> m exp         function call with single result
# lst n text exp1...expn -> m exp1...expm call with list of results
# terminate                               end of execution
#
# negative m value indicates execution error
# in this case 'lst' returns a single line denoting the error
#
# -----------------------------------------------------------------------------

# protocol identifier
dist_version := "HaskellGAP 1.0";;
dist_versionLength := 1022;;
out_value := 0;

# -----------------------------------------------------------------------------

socket_in := InputTextUser();
socket_out := OutputTextUser();
debug_file := "/tmp/debugAbyd";

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# read line of input, check for errors
# -----------------------------------------------------------------------------
dist_readline := function()
  local
    len, line0, line01;
    line0 := "";

  line0 := ReadLine(socket_in);


  if line0 = fail then
    Error("unexpected end of input socket");
AppendTo(debug_file,"unexpected end of input socket\n\n");
  fi;

#  # GAP is setup to read from stdin at most 1022 characters per line
#  # thus we join subsequent truncated lines
#  len := Length(line0);
#
#  while len < 1022 do
#    line01 := ReadLine(socket_in);    
#    if line01 = fail then
#      Error("unexpected end of input socket");
#    fi;
#    Append(line0, line01);
#    len := Length(line0);
#  od;

if line0[Length(line0)] = '\n' then
    Unbind( line0[Length(line0)] );
fi;
  return (line0);

end;

# -----------------------------------------------------------------------------
# parse and print functions
# -----------------------------------------------------------------------------
dist_parse := function ()
  # text -> internal
  return (dist_readline());
end;

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

dist_print := function (value)
  # internal -> text
  AppendTo( debug_file, dist_EvalString(value)); 
  Print(dist_EvalString(value), "\n");
end;;

dist_read := function()
  # exp -> internal
  local str;
  str := dist_readline();
  AppendTo(debug_file, "read: ", str, "\n"); 
  AppendTo(debug_file, "parse:", str, "\n");
  return str;
end;;

dist_write := function (value)
  # internal -> exp
  AppendTo(debug_file, "write: ", value, "\n");
  Print(value,"\n");
end;;

dist_print := dist_write;

# -----------------------------------------------------------------------------
# server loop
# -----------------------------------------------------------------------------
dist_server := function(  )
  local
    line, value, fun, n, i, t, atable, atable_size, tmp_atable;

  # server loop
  atable := [];
  tmp_atable :=[];
  line := "";
  while true do

    # message key
repeat
AppendTo(debug_file, "READ LINE ........... \n");
        line := dist_readline(  );
AppendTo(debug_file,"What is in line =>", line, "\n");
until not IsEmpty(line) and not line="0";
AppendTo( debug_file, "$$line: ", line, "\n" );

    # text to expression
    if line = "in" then
      value := dist_readline();
      Add(atable,value);
      dist_print(0);
      atable_size := Size(atable) ;
AppendTo(debug_file, "examine atable_size=>",atable_size,"\n");
      if atable_size > 1 then
        dist_write(0);          
##>        dist_write(value);          
      fi;      


    # expression to text
    elif line = "out" then
      value :=  dist_read();
AppendTo(debug_file, "From out, value >>>>",value, "\n");
      dist_print(0);
      dist_print(value);

    # evaluate function and write result
    elif line = "fun" or line = "lst" then
      # ask number of arguments
AppendTo(debug_file,"atable_size ==> ", atable_size, "\n");
      line := dist_readline();
      n := Int( line );
AppendTo(debug_file, "n:", n, "\n");
      line := dist_readline();
      fun := SplitString( line, ' ' )[1];
##AppendTo(debug_file,"atable ==> ", atable, "\n");
AppendTo(debug_file, fun , "\n");
AppendTo(debug_file,"Size(atable) ==> ", Size(atable), "  Size(atable)-n+1",Size(atable)-n+1 , "\n");
      if atable <>  [ "ready" ] then 
        tmp_atable := [];
        for i in [(Size(atable)-n+1) .. Size(atable)] do
            Add(tmp_atable,atable[i]);
        od;
AppendTo(debug_file,"tmp_atable ==> ", tmp_atable, "\n");
        value := CallFuncList( EvalString( fun ), List( tmp_atable, dist_EvalString ) );
      fi;
AppendTo(debug_file, " My value ==> " , value ,"\n");

      dist_print(0);

      # "fun"
AppendTo(debug_file, "value =" ,value, "\n");
      dist_write(value);

#      # "lst"
#      dist_print(Length(value));
#
#          for i in [1 .. Length(value)] do
#            AppendTo(debug_file, i, "\n"); 
#            dist_print(value[i]);
#          od;

    # terminate execution
    elif line = "terminate" then
      return "terminate";

    # unknown message key
    else
AppendTo(debug_file, "GAP process error: invalid message key ", line,"\n");
      return fail;

    fi;
  od;
end;


# -----------------------------------------------------------------------------
# main program
# -----------------------------------------------------------------------------
dist_main := function()
  local result, debug;

  if not IsStream( socket_in ) or IsClosedStream( socket_in )  then
      socket_in := InputTextUser(  );
      socket_out := OutputTextUser(  );
#      socket_in := InputTextFile( "input_file" );
#      socket_out := OutputTextFile( "output_file" );
    repeat
      debug:=ReadAllLine(socket_in);
      AppendTo(debug_file, "socket");
      AppendTo(debug_file, debug);
    until debug=fail;

  fi;

  # signal readiness
  WriteLine(socket_out, "ready");
  
  # name of file for error messages
#  dist_readline();


  # execute server loop and catch errors
  result := dist_server(  );
  if result = "terminate" then
    CloseStream( socket_in );
    CloseStream( socket_out );
  fi;

  # check for error
  if result = fail then
    AppendTo(debug_file, "GAP process error");
  fi;
end;

# -----------------------------------------------------------------------------
# start execution
# -----------------------------------------------------------------------------

LogTo("/tmp/gap_debug.txt");
AppendTo(debug_file, "starting execution of start.g\n");

dist_main();

AppendTo(debug_file, "ending execution of start.g\n");
LogTo();

# -----------------------------------------------------------------------------
# $Id: start.g,v 1.23 2006/09/08 08:38:40 gapchron Exp $
# -----------------------------------------------------------------------------
