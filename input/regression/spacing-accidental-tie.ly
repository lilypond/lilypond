\version "2.12.0"

\header {
  texidoc = "Horizontal spacing works as expected on tied notes with
accidentals. No space is reserved for accidentals that end up not being printed,
but accidentals that are printed don't collide with anything."
}

\paper { ragged-right = ##t }

\relative c'
{ \time 1/4
  cis16 cis cis cis~
  cis cis cis cis
  c c c c \break

  cis16 cis cis cis~
  cis! cis cis cis
  c c c c \break

  cis cis cis cis~ \break
  cis
}
