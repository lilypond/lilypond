
\version "2.3.8"

\header { texidoc = "@cindex Textscript
There are different fonts and glyphs to be used with @code{\markup} command. "
}

\score{
  \relative c''{
    \override TextScript  #'font-shape = #'upright
    c1^\markup { \dynamic "p" "ma sosten." }  
    c^\markup \huge "ABCD" 
    \override TextScript  #'font-series = #'bold
    c^\markup { \bold "Dal" " " \raise #0.8 \musicglyph #"scripts-segno" }
    c^\markup \huge "ABCD"
  }
	\paper{ }
}

