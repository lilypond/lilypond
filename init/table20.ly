% table20.ly
%
% spacing info for LilyPond. Do not edit this.
% It has a lot of hard-wired stringconstants
%

table_twenty = 
\symboltables {

   \texid 	"\input lilyponddefs \musixtwentydefs"

    % index TeXstring, 	xmin xmax ymin ymax

     "style" = \table {
		"roman"	"\settext{%}"	0.0\pt	7.5\pt	0.0\pt	10.0\pt
		"italic"	"\setitalic{%}"	0.0\pt	0.0\pt	0.0\pt	8.0\pt
		"dynamic"	"\setdynamic{%}"	0.0\pt	0.0\pt	0.0\pt	8.0\pt
     }
     "dynamics" = \table {

	"mf" "\dynmf"
	"fff" "\dynfff"
	"ff" "\dynff"
	"f" "\dynf"
	"mp" "\dynmp"
	"p" "\dynp"
	"pp" "\dynpp"
	"ppp" "\dynppp"
	"fp" "\dynfp"
	"sf" "\dynsf"
	"sfz" "\dynsfz"

	}
     "align" = \table {
		"-1"	"\leftalign{%}"
		"0"	"\centeralign{%}"
		"1"	"\rightalign{%}"
 	}


    "clefs" = \table {
	"violin"	"\violinclef" 	0.0\pt	16.0\pt	-12.5\pt	22.5\pt
	"bass"	"\bassclef" 		0.0\pt	16.0\pt	0.0\pt	20.0\pt
	"alto"	"\altoclef"	 	0.0\pt	16.0\pt	0.0\pt	20.0\pt
	"tenor"	"\altoclef"	 	0.0\pt	16.0\pt	0.0\pt	20.0\pt
	"violin_change"	"\cviolinclef" 	0.0\pt	16.0\pt	-12.5\pt	22.5\pt
	"bass_change"	"\cbassclef"	0.0\pt	16.0\pt	0.0\pt	20.0\pt
	"alto_change"	"\caltoclef" 	0.0\pt	16.0\pt	0.0\pt	20.0\pt
	"tenor_change"	"\caltoclef" 	0.0\pt	16.0\pt	0.0\pt	20.0\pt
    }


    "slur" = \table {
	"whole"	"\slurchar%{%}"	0.0\pt	0.0\pt	0.0\pt	0.0\pt
	"half"	"\hslurchar%{%}"	0.0\pt	0.0\pt	0.0\pt	0.0\pt
    }


    "streepjes" = \table {
	"toplines"	"\\topledgerlines{%}{%}"	-3.0\pt	9.0\pt	0.0\pt	0.0\pt
	"botlines"	"\botledgerlines{%}{%}"	-3.0\pt	9.0\pt	0.0\pt	0.0\pt
    }

    "bars" = \table {
	"empty"	"\emptybar"
	""	""			0.0\pt	0.0\pt	0.0\pt	16.0\pt
	"|"	"\maatstreep{%}"	0.0\pt	0.64\pt 	0.0\pt	20.0\pt
	"||"	"\doublebar{%}"		0.0\pt	4.0\pt	0.0\pt	20.0\pt
	"|."	"\finishbar{%}"		0.0\pt	2.0\pt	0.0\pt	20.0\pt
	".|"	"\startbar{%}"		0.0\pt	4.0\pt	0.0\pt	20.0\pt
	":|"	"\repeatbar"		-8.0\pt	0.0\pt	0.0\pt	20.0\pt
	"|:"	"\startrepeat"		0.0\pt	8.0\pt	0.0\pt	20.0\pt
	":|:"	"\repeatbarstartrepeat"	0.0\pt	16.0\pt	0.0\pt	20.0\pt
    }

    "meters" = \table {
	"C"	"\fourfourmeter"		0.0\pt	10.0\pt	-5.0\pt	5.0\pt
	"C2"	"\allabreve"		0.0\pt	10.0\pt	-5.0\pt	5.0\pt
    }

    % dims ignored for this table
    "param" = \table {
	 "brace"    "\pianobrace{%}"	0.0\pt	0.0\pt	0.0\pt	0.0\pt
	 "meter"	"\generalmeter{%}{%}"	0.0\pt	10.0\pt	-5.0\pt	5.0\pt
	 "linestaf"	"\linestafsym{%}{%}"
	 "stem"	"\stem{%}{%}"
	 "fill"	"\hbox{}"
	% ugh. 8\pt
	 "crescendosym" "\crescendosym{%}"	0.0\pt	0.0\pt	-3.0\pt	3.0\pt
	 "decrescendosym" "\decrescendosym{%}"	0.0\pt	0.0\pt	-3.0\pt	3.0\pt
     }

    "flags" = \table {
	"u3"	"\eighthflag"		0.0\pt	5.0\pt	0.0\pt	0.0\pt	
	"u4"	"\sixteenthflag"		0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"u5"	"\\thirtysecondflag"	0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"u6"	"\sixtyfourthflag"	0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"u7"	"\hundredtwentyeighthflag"	0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"d3"	"\deighthflag"		0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"d4"	"\dsixteenthflag"		0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"d5"	"\dthirtysecondflag"	0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"d6"	"\dsixtyfourthflag"	0.0\pt	5.0\pt	0.0\pt	0.0\pt
	"d7"	"\dhundredtwentyeighthflag"	0.0\pt	5.0\pt	0.0\pt	0.0\pt
     }

  "beamslopes" = \table {
	"slope"		"\beamslope{%}{%}"  2.0\pt 64.0\pt 0.0\pt 0.0\pt	
	"horizontal"	"\rulesym{%}{%}"	
     }
     
    % ugh what's our outdir called?
    \include "font-en-tja20.ly"
}

