\version "2.23.10"

\header {
    texidoc = "In French style beaming, the stems do not go between beams."
}

\layout { ragged-right= ##t }

\relative
{
    \override Stem.french-beaming = ##t
    c'16[ c c c]
    c8[ c16 e16 f16 g16 g8]
    f'32_1 c_3 a64_> g_>]
    a,32[ b c a16]
    \noBreak
    e'32[ f g a16 r]
    \override Beam.grow-direction = #LEFT
    \featherDurations 2/1
    { c,16[ c c c c c c c] }
    \override Beam.grow-direction = #RIGHT
    \featherDurations 2/3
    { d32[^1 e^2 f^3 g^4 a^5] } s64
}
