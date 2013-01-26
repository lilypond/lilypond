
\header{

  texidoc = "Tuplet brackets are also skipped with
@code{skipTypesetting}."

}


\version "2.17.11"
\paper {
  ragged-right = ##T
}

\new Staff \relative c' {
  \set Score.skipTypesetting = ##t
  \tuplet 3/2 { c8 c c } \tuplet 3/2 { c c c }
  \tuplet 3/2 { c c c } \tuplet 3/2 { c c c}
  \set Score.skipTypesetting = ##f
  d1 \break
  c
}
