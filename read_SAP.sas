%let dir=H:\Not2migrate\Programming\Jthu\disn;

libname hf "&dir";
filename sap "&dir\sap_simple.txt";

data sap;
	infile sap;
	input;

	line=left(_infile_);
	if line = '' then delete;
run;


data hf0 (keep = line newline Table_Type tag tagN) ;
	  set sap;

	  if _n_=1 then do;
			retain queTable queFigure queList queSymbol;

			Table="/^Table (\d+)\.(\d+)\:/";
			Figure="/^Figure (\d+)\.(\d+)\:/";
			List="/^Listing (\d+)\.(\d+)\:/";
			Symbol="/^\<|\>$/";

			queTable  = prxparse(Table);
			queFigure = prxparse(Figure);
			queList   = prxparse(List);
			queSymbol = prxparse(Symbol);

			if missing(queTable) then do;
			   putlog "ERROR: Invalid regexp" Table;
			   stop;
			end;

			if missing(queFigure) then do;
			   putlog "ERROR: Invalid regexp" Figure;
			   stop;
			end;

			if missing(queList)  then do;
			   putlog "ERROR: Invalid regexp" List;
			   stop;
			end;

			if missing(queSymbol) then do;
			   putlog "ERROR: Invalid regexp" Symbol;
			   stop;
			end;
	  end;

	  queTableN  = prxmatch(queTable ,line);
	  queFigureN = prxmatch(queFigure,line);
	  queListN   = prxmatch(queList  ,line);
	  queSymbolN = prxmatch(queSymbol,line);


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

	  else if queFigureN > 0 then do;
			call PRXsubstr (queFigure, line, position, length);
			newline = substr(line, position, length-1);
			Table_Type = 'F';
			tag="title_id";
			tagN=1;
			output;
			newline = substr(line, position+length+1);
			Table_Type = '';
			tag="title";
			tagN=2;
			output;
	  end;

	  else if queListN > 0 then do;
			call PRXsubstr (queList, line, position, length);
			newline = substr(line, position, length-1);
			Table_Type = 'L';
			tag="title_id";
			tagN=1;
			output;
			newline = substr(line, position+length+1);
			Table_Type = '';
			tag="title";
			tagN=2;
			output;
	 end;

	 else if queSymbolN > 0 then delete;

	 else do;
		newline = line;
		tag="footnoot";
		tagN=3;
		output;
	end;
run;

data _null_;
	set hf0;

	obs=_N_;
	len= length(newline);

	if len>200 then do;
		putlog "ERROR: length of text above 200";
		put obs= newline= len=;
	end;
run;

data _null_;
    retain max;
    do until(last);
            set hf0(where=(tagN ne 2)) end=last;
            count+ifn(tagN=1,-count,1);
            max=max(count,max);
			call symputx("max",max);			
    end;
run;

%*put &max;

Data _parRes_cl(drop=pos);
	Set hf0;
		If Table_Type = 'T' Then do;
			pos = find(Newline, '9.');
			if pos > 0 then do ;
				Table_TypeNum = 'T09';
				subID = substr(Newline, pos+2);
			end;

			pos = find(Newline, '3.');
			if pos > 0 then do;
				Table_TypeNum = 'T15';
				subID = substr(Newline, pos+2);
			end;

			pos = find(Newline, '7.');
			if pos > 0 then do;
				Table_TypeNum = 'T01';
				subID = substr(Newline, pos+2);
			end;
      end;

	else if Table_Type = 'F' Then do;
		pos = find(Newline, '9.');
		if pos > 0 then do ;
			Table_TypeNum = 'F09';
			subID = substr(Newline, pos+2);
		end;
	end;

	Else if Table_Type = 'L' Then Do;
		pos = find(Newline, '16.');
		if pos > 0 then do;
			Table_TypeNum = 'L16';
			subID = substr(Newline, pos+3);
		end;
	End;
Run;


Data _parRes_cl_1;
	Set  _parRes_cl;
	Retain pre_titleID;
	If _N_ = 1 Then Pre_titleID = Newline;
	Else If Table_Type in ('T','F','L') Then Do;
		Pre_titleID = Newline;
	End;
Run;



Data _parRes_cl_3;

	Set  _parRes_cl_1;
	Retain Type_i;
	
	Retain Table_TypeNum_1  SubID_1 Table_Type_1;

	If Table_TypeNum ne '' Then Table_TypeNum_1 = Table_TypeNum;

	If Table_type ne '' Then do;
		Table_type_1 = Table_Type;
		TYPE_i = 1; 
		TYPE ='T01';
	End;
	If Table_Type = '' Then do;
		Type_i = Type_i +1;
		If Type_i = 2 Then Type = 'T02';
		Else Type = compress('F0' || put(Type_i -2 , $3.));
	End;

	If SubID ne '' Then SubID_1 = SubID;

Run;


/*shar*/



data headfoot;
set _parRes_cl_3;
    if upcase(substr(type,1,1))="T" then t_f=1;
    if upcase(substr(type,1,1))="F" then t_f=2;
    seq=input(substr(type,2),3.);	/*convert char into numeric*/

 
  run;

proc sort data=headfoot;
  by pre_titleID t_f seq;
run;

data headfoot(drop=label t_f seq i);
  retain t1-t20  ft1-ft20 ;
  length   t1-t20 $600. ft1-ft20 $600.;
    length tableno $15. tableid $10. pgmid $10. pgmer $8. validat $10. stats $10. type $3. indent 8. label $600. ;

  array t1t(20) $600. t1-t20;
  array f1f(20) $600. ft1-ft20   ;
  set headfoot;
  by tableno t_f seq;
  if first.tableno then do i=1 to 20;
    t1t(I)=" ";
    f1f(I)=" ";
  end;
  if t_f=1 then do;
         if seq=1 then t1t(seq)=left(label);
    else if seq>1 then t1t(seq+1)=left(label);
  end;

  if t_f=2 then do;
      f1f(seq)=left(label);
  end;

 if last.tableno then output;
run;

%macro putnull(x,s,e);
	%do i=%eval(&s) %to %eval(&e);
	  	&x&i=put("",$600.);;
	%end;
%mend;

%macro rep(x,n,note);
	%do i=1 %to %eval(&n);
   		&x&i "TLF &note &i" length 600 format $600.,
	%end;
%mend;

data samp;
	LENGTH objno $20. objlabel $600. STATREF $150.;
	set headfoot;   
	objno=PUT(T1,$20.);
	objlabel=left(trim(put(t3,$600.)))||" "||left(trim(put(t4,$600.)))||" "||left(trim(put(t5,$600.)));
	STATREF=put("",$150.);
    tableno = put(pgmid, $10.);
run;

data samp2;
	set samp(drop=type);
	length T1-T5 /*FT21-FT50*/ COM1-COM5 $600. type $600.;
	format T1-T5 /*FT21-FT50*/ COM1-COM5 $600. type $600.;
    %putnull(T,1,5);
/*    %putnull(FT,21,50);*/
    %putnull(COM,1,5);
	if upcase(objno) =:'FIGURE' then type='Figure';
		else if upcase(objno) =:'TABLE' then type='Table';
		else if upcase(objno) =:'LIST' then type='Listing';
run;

proc sql;
	create table out.headfoot as
	select objno    "Object Number",
	       objlabel "Object Label (Title)",
		   tableno  "TLF FileName",
		   tableid  "TLF ID",
		   pgmid    "Program ID",
		   %rep(T,5,FirstPageComments)
		   %rep(FT,20,LastPageComments/Footnotes)
		   statref  "Stat Reference",
		   /*%rep(COM,5,General Comments)*/
		   pgmer "Programmer ID",
		   validat "Validator ID",
		   stats "Statistician ID",
		   type "TLF type"
	from samp2;
quit;
