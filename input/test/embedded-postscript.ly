\header {
  texidoc = "By inserting the @TeX{} command \embeddedps, you can
insert postscript directly into the output.

TODO: make molecule-callback to do this. 
  ";
}

\score {
  \notes \relative c'' {
    a-#"\\embeddedps{3 4 moveto 5 3 rlineto stroke}"
    -#"\\embeddedps{ [ 0 1 ] 0 setdash 3 5 moveto 5 -3 rlineto stroke}"
    b-#"\\embeddedps{3 4 moveto 0 0 1 2 8 4 20 3.5 rcurveto stroke}"
    s2
    a'1
  }
  \paper { linewidth = 70 * \staffspace; }
}
