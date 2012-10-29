\header{
  texidoc = "An arpeggio following a bar gets space"

}

\version "2.17.6"

\layout{ragged-right=##t}
\new Staff {

  \override Score.NonMusicalPaperColumn.stencil = #ly:paper-column::print
  \override Score.NonMusicalPaperColumn.layer = #1
  
  s1
  <b' c'' c'>2\arpeggio
}
