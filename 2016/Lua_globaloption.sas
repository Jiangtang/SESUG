/*
Usage Note 57993: Global options are not honored across programs that 
start up "side sessions" of SAS®
http://support.sas.com/kb/57/993.html
*/

data temp;
   do i=1 to 10;
      output;
   end;
run;


data _null_;
   rc = dosubl('options obs=5; data _null_; set temp; put _all_; run;');
   rc = dosubl('               data _null_; set temp; put _all_; run;');
run;

data _null_;
   set temp;
   put _all_;
run;
