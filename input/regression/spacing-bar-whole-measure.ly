\header{
  texidoc = "Notes that fill a whole measure are preceded by extra space. "
  
  }

\version "2.12.0"

\layout{
  ragged-right=##t
}

\new Staff \relative {
  \override Score.NonMusicalPaperColumn #'stencil = #ly:paper-column::print
  \override Score.NonMusicalPaperColumn #'layer = #1
  
  \time 4/4
  s1
  c2. c4
  \time 3/4
  s2.
  c2.
}
