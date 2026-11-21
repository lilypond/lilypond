\version "2.27.6"

\header {
  texidoc = "The @code{strictBeatBeaming} context property selects the policy
for fractional beams.  See the internals reference for a detailed description.
In these tests, its value is printed in the margin."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  \context {
    \Staff
    \hide StaffSymbol
    \remove Clef_engraver
  }
}

testRhythm = \fixed c' {
  f8. 16 16 16 16 8.
  \section
  \sectionLabel \markup \small "manual beaming"
  f8.[ 16] 16[ 16 16 8.]
  f8.[ 16 16 16] 16[ 8.]
  f8.[ 16 16 16 16 8.]
  \section
}

runTest =
#(define-music-function (beat-structure strictness)
  (optionally-grouped-beat-structure? boolean-or-symbol?) #{
  \new Staff \with {
    strictBeatBeaming = #strictness
    instrumentName = #(format #f "~a" strictness)
  } <<
    \tempo #(string-join (map number->string beat-structure) ",")
    \time #beat-structure 5/8
    \testRhythm
  >>
  #} )

\runTest 2,3 ##f
\runTest 2,3 #'on-beat
\runTest 2,3 ##t

\runTest 3,2 ##f
\runTest 3,2 #'on-beat
\runTest 3,2 ##t

\runTest 5 ##f
\runTest 5 #'on-beat
\runTest 5 ##t
