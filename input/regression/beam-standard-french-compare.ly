\version "2.21.0"

\header {
    texidoc = "Beam positioning and placement of articulations, fingerings,
tuplet numbers, and slurs must be identical in standard and French beaming
style."
}

testmusic = \relative
{
    c'32[^^ c^^ c^^ c^^]
    \tuplet 3/2 { e'16 c a }
    f16[_1 g_2 a32_3 b_1 c_2 d_3 e_4 f]_5
    \override Beam.auto-knee-gap = #1
    a,,32[ a a'' a]
    a, a  a a^( a a a16) a
}

\score {
  <<
    \new Staff \testmusic
    \new Staff \with { \override Stem.french-beaming = ##t } \testmusic
  >>
}
