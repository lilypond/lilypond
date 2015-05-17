\version "2.19.21"

\header {
  texidoc = "
Dots on harmonic note heads can be shown by setting the property
@code{harmonicDots}.
"
}

\relative {
  r4 <bes' es\harmonic>2.
  \set harmonicDots = ##t
  r4 <bes es\harmonic>2.
}
