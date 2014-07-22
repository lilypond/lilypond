\version "2.19.11"
\header {
  texidoc ="Lyrics can be aligned to a @code{NullVoice} context,
  which prints no notes, with the usual mechanisms for melismata."
}

\score { <<
  \new Staff <<
    { c''4. g'8 c''2 | g'4( f' g'2) } \\
    { c'4 b c'8 e' g' e' | c'1 }
    \new NullVoice = "nv" {
      \autoBeamOff c4 r16 b,16~ b,8 c8[ e8 g8 e8] | g4( f4) g2 }
  >>
  \new Lyrics \lyricsto "nv" { free a -- lign -- ment }
>> }
