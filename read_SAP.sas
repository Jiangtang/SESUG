
data hf0 (keep = line newline Table_Type tag tagN) ;
	  set sap;

	  if _n_=1 then do;
			retain queTable queFigure queList queSymbol;

			Table="/^Table (\d+)\.(\d+)\:/";

			queTable  = prxparse(Table);

			if missing(queTable) then do;
			   putlog "ERROR: Invalid regexp" Table;
			   stop;
			end;
	  end;

	  queTableN  = prxmatch(queTable ,line);

	  if queTableN > 0  then do ;
			call PRXsubstr (queTable, line, position, length);
			newline = substr(line, position, length-1);
			Table_Type = 'T';
			tag="title_id";
			tagN=1;
			output;
			newline = substr(line, position+length+1);
			Table_Type = '';
			tag="title";
			tagN=2;
			output;
	  end;	  
run;
