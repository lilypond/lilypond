
\version "2.1.22"

\header { texidoc = "@cindex Textscript
Test font selection and scm text markup. "
}

\score{
  \notes\relative c''{
    \override TextScript  #'font-shape = #'upright
    c1^\markup { \dynamic "p" "ma sosten." }  
    c^\markup \huge "ABCD" 
    \override TextScript  #'font-series = #'bold
    c^\markup { \bold "Dal" " " \raise #0.8 \musicglyph #"scripts-segno" }
    c^\markup \huge "ABCD"
  }
	\paper{ }
}

