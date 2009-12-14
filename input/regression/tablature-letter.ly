\version "2.13.9"

\header { texidoc = "A sample tablature with lettered tab,
using fretLabels to modify the fret letters.

By default, letters are drawn sequentially from the alphabet,
but if the context property fretLabels is defined, these are
substituted.  If specified, the length of fretLabels must be
sufficient to label all the frets used.  A warning is issued
if the length is too short.
"
}

notes = \relative c' {
  \time 3/4
  <f d>4. <bes>8 <g e>4
  \set fretLabels = #`("a" "b" ,(markup #:italic #:smaller "c"))
  <f d>4. <bes>8 <g e>4
  \set fretLabels = #`(,(markup #:with-color red "a")
                       "b"
                       ,(markup #:italic #:smaller "c"))
  <f d>4. <bes>8 <g e>4
  \set fretLabels = #'("α" "β" "γ")
  <f d>4. <bes>8 <g e>4
}

\score {
  \new TabStaff
  \with {
    stringTunings = #'(17 14 9 5 2 -3)
    tablatureFormat = #fret-letter-tablature-format
  }
  \new TabVoice {
    \notes
  }
}



