% table16.ly
%
% spacing info for LilyPond. Do not edit this if you're not a guru.
% It has a lot of hard-wired stringconstants
%


table_sixteen= 
\symboltables {
   \font 	"feta16.afm"

   % index TeXstring, 	xmin xmax ymin ymax

     "style" = \table {
		"bold"	"\\setbold{%}"			0.0\pt	7.50\pt	0.0\pt	8.0\pt
		"dynamic"	"\\setdynamic{%}"	0.0\pt	0.0\pt	0.0\pt	8.0\pt
		"finger"	"\\setfinger{%}"	0.0\pt	2.0\pt	0.0\pt	4.0\pt
		"italic"	"\\setitalic{%}"	0.0\pt	4.\pt	0.0\pt	8.0\pt
		"typewriter"	"\\settypewriter{%}"	0.0\pt	4.\pt	0.0\pt	8.0\pt
		"large"	"\\setlarge{%}"			0.0\pt	9.50\pt	0.0\pt	10.0\pt
		"number"	"\\setnumber{%}"	0.0\pt	6.0\pt	0.0\pt	8.0\pt
		"roman"	"\\settext{%}"			0.0\pt	4.5\pt	0.0\pt	8.0\pt
     }
     "dynamics" = \table {
	"mf" "\\dynmf"
	"fff" "\\dynfff"
	"ff" "\\dynff"
	"f" "\\dynf"
	"mp" "\\dynmp"
	"p" "\\dynp"
	"pp" "\\dynpp"
	"ppp" "\\dynppp"
	"fp" "\\dynfp"
	"sf" "\\dynsf"
	"sfz" "\\dynsfz"
	}
     "align" = \table {
		"-1"	"\\leftalign{%}"
		"0"	"\\centeralign{%}"
		"1"	"\\rightalign{%}"
 	}


     "bars" = \table {
	"empty"	"\\emptybar"
	""	""			0.0\pt	0.0\pt	0.0\pt	16.0\pt
	"|"	"\\maatstreep{%}"	0.0\pt	0.64\pt 0.0\pt	16.0\pt
	"||"	"\\doublebar{%}"		0.0\pt	4.0\pt	0.0\pt	16.0\pt
	"|."	"\\finishbar{%}"		-4.0\pt	0.0\pt	0.0\pt	16.0\pt
	".|"	"\\startbar{%}"		0.0\pt	4.0\pt	0.0\pt	16.0\pt
	".|."	"\\fatdoublebar{%}"		0.0\pt	10.0\pt	0.0\pt	20.0\pt
	":|"	"\\repeatbar"		-8.0\pt	0.0\pt	0.0\pt	16.0\pt
	"|:"	"\\startrepeat"		0.0\pt	8.0\pt	0.0\pt 16.0\pt
	":|:"	"\\repeatbarstartrepeat"	0.0\pt	16.0\pt	0.0\pt	16.0\pt

     }

     "meters" = \table {
	"C"	"\\fourfourmeter"		0.0\pt	10.0\pt	-5.0\pt	5.0\pt
	"C2"	"\\allabreve"		0.0\pt	10.0\pt	-5.0\pt	5.0\pt
     }

     % dims ignored for this table
     "param" = \table {
	"brace"	"\\pianobrace{%}"	0.0\pt	0.0\pt	32.0\pt	80.0\pt
	"bracket"	"\\staffbracket{%}"	0.0\pt	4.0\pt	16.0\pt	128.0\pt
	"time_signature"	"\\generalmeter{%}{%}"	0.0\pt	10.0\pt	-8.0\pt	8.0\pt
	"rule" "\\rulesym{%}{%}"
	"stem"	"\\stem{%}{%}"
	"fill"	"\\hbox{}"
     }


     "beamslopes" = \table {
	"slope"		"\\beamslope{%}{%}"  2.0\pt 64.0\pt 0.0\pt 0.0\pt
	"horizontal"	"\\rulesym{%}{%}"	
     }
}

