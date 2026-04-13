\version "2.25.35"

\header {
  categories = "Rhythms"

  texidoc = "
Normally, LilyPond refuses to automatically break a line at places
where a beam crosses a bar line. This behavior can be changed by
setting the @code{Beam.breakable} property to @code{#t}.

This property does not affect manual breaks inserted with commands like
@code{\\break}.
"

  doctitle = "Beams across line breaks"
} % begin verbatim


music = {
  \*8 c8
  c8 \*7 { c[ c] }  c
  \*8 c8
}

\relative c'' {
  <>^\markup { \typewriter Beam.breakable set to \typewriter "#t" }
  \override Beam.breakable = ##t
  \music
}

\relative c'' {
  <>^\markup { \typewriter Beam.breakable not set }
  \music
}

\paper {
  line-width = 100\mm
}
