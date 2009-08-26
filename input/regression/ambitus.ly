\version "2.12.0"

\header {
  texidoc = "Ambitus indicate pitch ranges for voices.

Accidentals only show up if they're not part of key
signature.  @code{AmbitusNoteHead} grobs also have ledger lines.
"
}

\layout {
  \context {
    \Voice
    \consists "Ambitus_engraver"
  }
}

<<
  \new Staff \relative c'{
    \time 2/4
    c4 f'
  }
  \new Staff \relative c' {
    \time  2/4
    \key d \major
    cis as'
  }
>>
