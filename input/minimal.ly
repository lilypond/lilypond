\notenames {
 c  = \melodic_request { -1 0 0 }
}

%default_paper = \paper{
paper_minimal = \paper{
 bla = 0.0;
 blo = 0.0;

 castingalgorithm = 0.0;
 linewidth = 15.0\cm;
 staffsize = 20.0\pt;
 interline = \linewidth / 14.0;
 notewidth = \interline;
 internote = \interline / 2.0;

 arithmetic_basicspace = 1.0;
 arithmetic_multiplier = 1.0;
 geometric = 0.0;

  Score = \translator {
   \type Score_engraver;
   \consists "Note_head_engraver" ;
  }

 bli = "hi";

 \symboltables { 
   \texid 	"\input lilyponddefs \musixtwentydefs"
   "balls"  = \table {
       "2" "\\quartball" 0.00\pt 3.63\pt -1.51\pt 1.51\pt 
       "2l" "\\quartledger" -1.65\pt 8.26\pt -0.50\pt 0.50\pt 
    }
    "param" = \table {
       "fill"	"\hbox{}"
%       "rule" "\rulesym{%}{%}"
    }
  }
}

default_paper = \paper{
	\paper_minimal
}

\score{
% \melodic_request { -1 0 0 }
 \melodic{ c'' }
 \paper{
  bla = 0.0;
 }
}

