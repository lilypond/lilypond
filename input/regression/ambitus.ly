\header {
    texidoc = "Ambituses indicate pitch ranges for voices.

Accidentals only show up if they're not part of key
signature. @code{AmbitusNoteHead} grobs also have ledger lines.

"
}
\version "2.11.51"

\layout {
    ragged-right = ##t
    \context {
	\Voice
	\consists Ambitus_engraver
    }
}

\relative
<<
    \new Staff {  \time  2/4 c4 f' }
    \new Staff \relative {
	\time  2/4
	\key d \major
	cis as'
    }
>>
