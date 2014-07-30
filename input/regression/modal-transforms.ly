\version "2.16.0"
\header {
    texidoc = "\modalTranspose, \retrograde, \inversion and
\modalInversion work for an octatonic motif."
}

cOctatonicScale = {
  c' d' ees' f'
  ges' aes' a' b'
}
motif = {
  c'8. ees'16( ges'8. a'16
  b'8.) aes'16 f'8. d'16
}

\score {
  \new Staff {
    \time 4/4
    <<
      {
        \motif
        \modalTranspose c' f' \cOctatonicScale \motif
        \retrograde \motif
        \modalInversion aes' b' \cOctatonicScale \motif
	\inversion aes' b' \motif
      }
      {
        s1-"Octatonic motif" |
        s1-"motif transposed from c to f" |
        s1-"motif in retrograde" |
        s1-"motif inverted around aes to b" |
	s1-"motif inverted exactly"
      }
    >>
  }
}
