\version "2.19.21"

\header {
    texidoc = "In French style beaming, the stems do not go between beams."
}

\layout { ragged-right= ##t }

\relative
{
    \override Stem.french-beaming = ##t
    c'16[ c c c]
    c8[ c16 e16 f16 g16 g8]
}


