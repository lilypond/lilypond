\version "2.19.21"

\header {
  texidoc = "Horizontal spacing works as expected on tied notes with
accidentals. No space is reserved for accidentals that end up not being printed,
but accidentals that are printed don't collide with anything."
}

\paper { ragged-right = ##t }

\relative
{ \time 1/4
  cis'16 cis cis cis~
  cis cis cis cis
  c c c c \break

  cis16 cis cis cis~
  cis! cis cis cis
  c c c c \break

  cis cis cis cis~ \break
  cis
}
