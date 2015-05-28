\header{
  texidoc = "Notes that fill a whole measure are preceded by extra space. "
  
  }

\version "2.19.21"

\layout{
  ragged-right=##t
}

\new Staff \relative {
  \override Score.NonMusicalPaperColumn.stencil = #ly:paper-column::print
  \override Score.NonMusicalPaperColumn.layer = #1
  
  \time 4/4
  s1
  c'2. c4
  \time 3/4
  s2.
  c2.
  \override Score.NonMusicalPaperColumn.full-measure-extra-space = #3.0
  s2.
  c2.
}
