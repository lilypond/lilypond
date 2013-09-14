\version "2.17.6"

\header {
  texidoc = "The gaps between an @code{AmbitusLine} and its
note heads are set by the @code{gap} property. By default,
@code{gap} is a function that reduces the gap for small intervals
(e.g. a fourth), so that the line remains visible."
}

\layout {
  \context {
    \Voice
    \consists "Ambitus_engraver"
  }
}

\new Staff {
  \time 2/4
  \override AmbitusLine.gap = #1
  c'4 g''
}

\new Staff <<
  \time 2/4
  { d'' g'' }
  \\
  { c' g' }
>>
