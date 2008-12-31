
\version "2.12.0"

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


\relative c''
{
  c4\startGroup\startGroup\startGroup
  c4\stopGroup
  c4\startGroup
  c4\stopGroup\stopGroup
  c4\startGroup
  c4\stopGroup\stopGroup
}


