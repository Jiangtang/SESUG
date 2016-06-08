data _null_;
    code = "data a; a=1;run;";
    call execute(code);
run;

data _null;
    code = "data b; a=1;run;";
    rc = dosubl(code);
run;



data _null_;
    code1 = "data a; a=1;run;";
    code2 = "data a; a=2;run;";

    call execute(code2);
    rc = dosubl(code1);    
run;


data _null_;
    code1 = "data a; a = 1; run;";
    code2 = "data b; set a; run;";
    
    rc = dosubl(code1);    
    call execute(code2);
run;
