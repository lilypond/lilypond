\header
{
texidoc = "An alternate method to make bass figures is to use
markup texts."
}
\version "2.1.1"

nat = \markup { \musicglyph #"accidentals-0" }
sh = \markup { \smaller \raise #0.6 \musicglyph #"accidentals-1" }
fl = \markup { \musicglyph #"accidentals--1" }

\score {
      \context Voice \notes {
	  \clef bass
	  \property Voice.TextScript \set #'font-family = #'number
	  \property Voice.TextScript \set #'font-size = #-6
	  \property Voice.TextScript \set #'baseline-skip = #1.4
	  
	  dis4_\markup { 6 }

	  c_\markup  { 7 }
	  d_\markup { \column < { 6 \sh } \bracket { \nat } >}
	  ais_\markup { \column < 6  5 \bracket { 3 \sh } >}
	  }
}      
