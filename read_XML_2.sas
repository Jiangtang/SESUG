data import3 (keep = text );
	 set xml;

	  if _n_=1 then do;
			retain quename rx1;
			data="/^\<text\> /";
			tag="s/<.*?>//";

			quename=prxparse(data);
			rx1=prxparse(tag);

			if missing(quename) then do;
				putlog "ERROR: Invalid regexp" data;
				stop;
			end;

			if missing(rx1) then do;
				putlog "ERROR: Invalid regexp" tag;
				stop;
			end;
	  end;

	  quenamen=prxmatch(quename,text);

	  if quenamen > 0  then do ;
			call prxchange(rx1,99,text);
			output;
	  end;
run;
