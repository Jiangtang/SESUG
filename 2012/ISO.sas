data ISO ;
	input time $40.;

    if _n_=1 then do;
      retain ISO_re;

	  ISO="/^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$/";
      ISO_re  = prxparse(ISO);

      if missing(ISO_re) then do;
         putlog "ERROR: Invalid regexp" time;
         stop;
      end;
    end;

    ISO_re_n  = prxmatch(ISO_re ,compress(time));

    if ISO_re_n > 0  then output;
datalines;
2008-08-30T01:45:36.123Z
2008-08-30T01:45:36.123Z07
2008-08-30T01:45:36
2008-08-30T01
2009-05-19 14:39:22+0600
;

