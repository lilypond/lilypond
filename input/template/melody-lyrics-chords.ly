\version "2.1.10"
\header
{
    texidoc = "Popsong format: chords, melody and lyrics."
}

melody = \notes \relative c'
{
    a b c d
}

text = \lyrics {
    Aaa Bee Cee Dee
}

accompaniment = \chords {
    a2 c2
}

\score {
  <<
    \context ChordNames \accompaniment
    \context Voice = one {
	\autoBeamOff
        \melody
    }
    \lyricsto "one" \new LyricsVoice \text
  >>
  \paper { }
  \midi  { }
}
