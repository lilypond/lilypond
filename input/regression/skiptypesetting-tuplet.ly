
\header{

  texidoc = "Tuplet brackets are also skipped with
@code{skipTypesetting}."

}


\version "2.19.21"
\paper {
  ragged-right = ##T
}

\new Staff \relative {
  \set Score.skipTypesetting = ##t
  \tuplet 3/2 { c'8 c c } \tuplet 3/2 { c c c }
  \tuplet 3/2 { c c c } \tuplet 3/2 { c c c}
  \set Score.skipTypesetting = ##f
  d1 \break
  c
}
