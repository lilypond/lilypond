\version "1.5.68"

\header {
texidoc="
Draw abritrary lines.  This brings LilyPond on par with Mup.
"
}

\score {
  \notes\relative c {
% globally positioned
%    a-#"\\special{ps: 0 0 moveto 200 200 lineto stroke}"
%    b-#"\\special{ps: 0 0 moveto 0 0 10 20 80 40 200 30 curveto stroke}"
    a''-#"\\embeddedps{3 4 moveto 5 3 rlineto stroke}"
    -#"\\embeddedps{ [ 0 1 ] 0 setdash 3 5 moveto 5 -3 rlineto stroke}"
    b-#"\\embeddedps{3 4 moveto 0 0 1 2 8 4 20 3.5 rcurveto stroke}"
    s2
    a'1
  }
  \paper {
    linewidth = 70.0*\staffspace
  }
}
