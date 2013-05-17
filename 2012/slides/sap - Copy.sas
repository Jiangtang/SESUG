filename sap "&dir\SESUG2012_SAP_Shell_dummy.txt";

data sap;
	infile sap;
	input ;

	line=left(_infile_);
	if line = '' then delete;
run;

data table;
	  set sap;

	  Table="/^Table (\d+)\.(\d+)\:/";
	  queTable  = prxparse(Table);

	  queTableN  = prxmatch(queTable ,line);
	  if queListN > 0  then output;	
run;
