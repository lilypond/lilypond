
\version "2.19.21"

\header {

  texidoc = "Note grouping events are used to indicate where
analysis brackets start and end.

@cindex bracket
@cindex note groups
@cindex music analysis
@cindex analysis

"
  
}


\layout {
  \context {
    \Staff \consists "Horizontal_bracket_engraver"
  }
  ragged-right = ##t
}


\relative
{
  c''4\tweak outside-staff-priority #802 \startGroup
    \tweak outside-staff-priority #801 \startGroup
    \startGroup
  c4\stopGroup
  c4\startGroup
  c4\stopGroup\stopGroup
  c4\startGroup
  c4\stopGroup\stopGroup
}


