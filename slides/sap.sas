data table;
	  set sap;
	  if _n_=1 then do;
			retain queTable;

			Table="/^Table (\d+)\.(\d+)\:/";
			queTable  = prxparse(Table);

			if missing(queTable) then do;
			   putlog "ERROR: Invalid regexp" Table;
			   stop;
			end;
	  end;

	  queTableN  = prxmatch(queTable ,line);
	  if queListN > 0  then output;	
run;
