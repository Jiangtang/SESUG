data import2 (keep = text );
	 set xml;

	  if _n_=1 then do;
			retain quename ;
			data="/^\<text\> /";

			quename=prxparse(data);

			if missing(quename) then do;
				putlog "ERROR: Invalid regexp" data;
				stop;
			end;
	  end;

	  quenamen=prxmatch(quename,text);

	  if quenamen > 0  then do ;
			output;
	  end;
run;
