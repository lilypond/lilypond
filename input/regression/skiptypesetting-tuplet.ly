
\header{

  texidoc = "Tuplet brackets are also skipped with
@code{skipTypesetting}."

}


\version "2.12.0"
\paper {
  ragged-right = ##T
}

\new Staff \relative c' {
  \set Score.skipTypesetting = ##t
  \times 2/3 { c8 c c } \times 2/3 { c c c }
  \times 2/3 { c c c } \times 2/3 { c c c}
  \set Score.skipTypesetting = ##f
  d1 \break
  c
}
