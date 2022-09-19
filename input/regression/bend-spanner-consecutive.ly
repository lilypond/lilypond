\version "2.23.14"

\header {
  texidoc = "Multiple consecutive @code{BendSpanner} grobs work.
Every @code{BendSpanner} following another one starts at the arrow head of the
previous one or at a @code{TabNoteHead}."
}

mus-consecutive-bend = {
  \tag top \textMark "consecutive bends up"
  c'4\3\^ d'\3 \^ e'\3 \^ fis'\3
  <f'\2 a'>4\^ <g' b'\1> \^ <a' cis''>2

  \tag top \textMark "consecutive bends down"
  fis'4\3 -\tweak details.successive-level #3 \^ e'\3\^ d'\3 \^ c'\3
  <a' cis''>4 -\tweak details.successive-level #2 \^ <g' b'>\^ <f' a'>2

  \tag top \textMark "with pre-bend-hold"
  <>_\markup \column { "Make first target" "parenthesized and visible" }
  \grace bes4\3 -\tweak style #'pre-bend-hold
                -\tweak details.target-visibility ##t
                -\tweak details.successive-level #3
                \^
  e'4\3 \^ d'\3 \^ c'\3 \^ bes\3 \break

  <>-"extreme"
  \grace cis'-\tweak style #'pre-bend-hold
             -\tweak details.target-visibility ##t
             -\tweak details.successive-level #3
             \^
  e'1\2\^ ees'\^ d'\^ cis'\^ \break
  d'\^  dis' \^ e'\2\^ ees' \^ e'\2
  \bar "|."
}

\score {
  \new StaffGroup
  <<
    \new Staff { \clef "G_8" \mus-consecutive-bend }
    \new TabVoice \removeWithTag top \mus-consecutive-bend
  >>
  \layout {
    \context {
      \Voice
      \omit StringNumber
    }
  }
}
