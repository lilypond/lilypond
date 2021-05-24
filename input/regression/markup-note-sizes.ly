\version "2.23.3"

\header {
  texidoc = "The @code{note-by-number} markup-command is robust with all kinds
of size changings.
For every @code{Stem} the vertical length and thickness prints reasonable."
}

\layout { \textLengthOn }

test =
\markup {
  "quavers:"
  \fontsize #-3 \note-by-number #3 #0 #1
  \note-by-number #3 #0 #1
  \fontsize #3 \note-by-number #3 #0 #1
}

testMusic = {
  \set Score.fontSize = #-3
  \override Score.MetronomeMark.font-size = #-3
  \override TextScript.font-size = #-3
  \tempo "Andante" 8 = 88
  b8_\test s2..
  \set Score.fontSize = #0
  \override Score.MetronomeMark.font-size = #0
  \override TextScript.font-size = #0
  \tempo "Andante" 8 = 88
  b8_\test s2..
  \set Score.fontSize = #3
  \override Score.MetronomeMark.font-size = #3
  \override TextScript.font-size = #3
  \tempo "Andante" 8 = 88
  b8_\test s2..
}

%% Some size settings below do not make much sense, though note-by-number
%% should be able to cope with them
#(set-global-staff-size 30)

\book {
  #(set-global-staff-size 10)

  \paper {
	#(layout-set-staff-size 25)
	indent = 0
	ragged-right = ##f
  }

  \markup { "toplevel markup in \\book: " \test }

  \score { \new Staff \testMusic \layout { #(layout-set-staff-size 10) } }

  \score { \new Staff \with { \magnifyStaff #3/4 } \testMusic }
  \score { \new Staff \testMusic }
  \score { \new Staff \with { \magnifyStaff #5/4 }\testMusic }

  \score { \new Staff \testMusic \layout { #(layout-set-staff-size 25) } }
}
