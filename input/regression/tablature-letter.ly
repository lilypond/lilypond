\version "2.19.21"

\header {
  texidoc = "
A sample tablature with lettered tab,
using fretLabels to modify the fret letters.

By default, letters are drawn sequentially from the alphabet,
but if the context property fretLabels is defined, these are
substituted.  If specified, the length of fretLabels must be
sufficient to label all the frets used.  A warning is issued
if the length is too short.
"
}

notes = \relative {
  \time 3/4
  <f' d>4. <bes>8 <g e>4
  \set fretLabels = \markuplist {"a" "b" \italic \smaller "c"}
  <f d>4. <bes>8 <g e>4
  \set fretLabels = \markuplist {\with-color #red "a"
                       "b"
                       \italic \smaller "c"}
  <f d>4. <bes>8 <g e>4
  \set fretLabels = \markuplist {"α" "β" "γ"}
  <f d>4. <bes>8 <g e>4
}

\score {
  \new TabStaff
  \with {
    stringTunings = \stringTuning <a d' f' a' d'' f''>
    tablatureFormat = #fret-letter-tablature-format
  }
  \new TabVoice {
    \notes
  }
}
