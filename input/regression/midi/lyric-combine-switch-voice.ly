\version "2.21.6"

\header {
  texidoc = "It is possible to switch the voice that @code{\\lyricsto}
follows."
}

\layout {
  indent = 0
  ragged-right = ##t
}

music = \fixed c' <<
  \new Staff <<
    \new Voice = Va \with \voiceOne { f4 s }
    \new Voice = Vb \with \voiceTwo { s4 e' }
  >>

  \new Lyrics \lyricsto Va {
    \set Lyrics.associatedVoice = Vb
    Hoo hah.
  }
>>

\score {
  \music
  \layout {}
  \midi {}
}
