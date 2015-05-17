\version "2.19.21"

\header {
  texidoc = "Ambitus indicate pitch ranges for voices.

Accidentals only show up if they're not part of key
signature.  @code{AmbitusNoteHead} grobs also have ledger lines.
The noteheads are printed in overstrike, so there's only one
visible; the accidentals are prevented from colliding. 
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
  \new Staff \relative {
    \time  2/4
    \key d \major
    cis' as'
  }
  \new Staff \relative {
    \time 2/4
    c'4 cis
  }
>>
