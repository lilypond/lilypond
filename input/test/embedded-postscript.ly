
\version "2.3.4"
\header {
  texidoc = "@cindex Embedded Postscript
By inserting the @TeX{} command \embeddedps, you can
insert postscript directly into the output.

  "
}


%
%TODO: make print-function to do this. 
% 

\score {
   \relative c'' {
    a-"\\embeddedps{3 4 moveto 5 3 rlineto stroke}"
    -"\\embeddedps{ [ 0 1 ] 0 setdash 3 5 moveto 5 -3 rlineto stroke}"
    b-"\\embeddedps{3 4 moveto 0 0 1 2 8 4 20 3.5 rcurveto stroke}"
    s2
    a'1
  }
  \paper { linewidth = 70 * \staffspace
%	raggedright = ##t 
	}
}

