% table16.ly
%
% spacing info for LilyPond. Do not edit this if you're not a guru.
% It has a lot of hard-wired stringconstants
%


table_sixteen= 
\symboltables {
   \font 	"feta16.afm"

   % index symbol #parameters  	xmin xmax ymin ymax

     "style" = \table {
		"bold"	"setbold"	1			0.0\pt	7.50\pt	0.0\pt	8.0\pt
		"dynamic"	"setdynamic"	1	0.0\pt	0.0\pt	0.0\pt	8.0\pt
		"finger"	"setfinger"	1	0.0\pt	2.0\pt	0.0\pt	4.0\pt
		"italic"	"setitalic"	1	0.0\pt	4.\pt	0.0\pt	8.0\pt
		"typewriter"	"settypewriter"	1	0.0\pt	4.\pt	0.0\pt	8.0\pt
		"large"	"setlarge"	1			0.0\pt	9.50\pt	0.0\pt	10.0\pt
		"number"	"setnumber"	1	0.0\pt	6.0\pt	0.0\pt	8.0\pt
		"roman"	"settext"	1			0.0\pt	4.5\pt	0.0\pt	8.0\pt
     }
     "dynamics" = \table {
	"mf" "dynmf"	0
	"fff" "dynfff"	0
	"ff" "dynff"	0
	"f" "dynf"	0
	"mp" "dynmp"	0
	"p" "dynp"	0
	"pp" "dynpp"	0
	"ppp" "dynppp"	0
	"fp" "dynfp"	0
	"sf" "dynsf"	0
	"sfz" "dynsfz"	0
	}
     "align" = \table {
		"-1"	"leftalign"	1
		"0"	"centeralign"	1
		"1"	"rightalign"	1
 	}


     "bars" = \table {
	"empty"	"emptybar"	0
	""	""	0			0.0\pt	0.0\pt	0.0\pt	16.0\pt
	"|"	"maatstreep"	1	0.0\pt	0.64\pt 0.0\pt	16.0\pt
	"||"	"doublebar"	1		0.0\pt	4.0\pt	0.0\pt	16.0\pt
	"|."	"finishbar"	1		-4.0\pt	0.0\pt	0.0\pt	16.0\pt
	".|"	"startbar"	1		0.0\pt	4.0\pt	0.0\pt	16.0\pt
	".|."	"fatdoublebar"	1		0.0\pt	10.0\pt	0.0\pt	20.0\pt
	":|"	"repeatbar"	0		-8.0\pt	0.0\pt	0.0\pt	16.0\pt
	"|:"	"startrepeat"	0		0.0\pt	8.0\pt	0.0\pt 16.0\pt
	":|:"	"repeatbarstartrepeat"	0	0.0\pt	16.0\pt	0.0\pt	16.0\pt

     }

     "meters" = \table {
	"C"	"fourfourmeter"	0		0.0\pt	10.0\pt	-5.0\pt	5.0\pt
	"C2"	"allabreve"	0		0.0\pt	10.0\pt	-5.0\pt	5.0\pt
     }

     % dims ignored for this table
     "param" = \table {
	"brace"	"pianobrace"	1	0.0\pt	0.0\pt	32.0\pt	80.0\pt
	"bracket"	"staffbracket"	1	0.0\pt	4.0\pt	16.0\pt	128.0\pt
	"time_signature"	"generalmeter"	2	0.0\pt	10.0\pt	-8.0\pt	8.0\pt
	"rule" "rulesym"	2
	"stem"	"stem"	2
	"fill"	"hbox{}"	0
     }


     "beamslopes" = \table {
	"slope"		"beamslope"	2  2.0\pt 64.0\pt 0.0\pt 0.0\pt
	"horizontal"	"rulesym"	2	
     }
}

