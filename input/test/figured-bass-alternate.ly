\header
{
texidoc = "An alternate method to make bass figures is to use
markup texts."
}
\version "2.1.22"

nat = \markup { \natural }
sh = \markup { \smaller \raise #0.6 \sharp }
fl = \markup { \flat }

\score {
      \context Voice \notes {
	  \clef bass
	  \override TextScript  #'font-family = #'number
	  \override TextScript  #'font-size = #-6
	  \override TextScript  #'baseline-skip = #1.4
	  
	  dis4_\markup { 6 }

	  c_\markup  { 7 }
	  d_\markup { \column < { 6 \sh } \bracket { \nat } >}
	  ais_\markup { \column < 6  5 \bracket { 3 \sh } >}
	  }
}      
