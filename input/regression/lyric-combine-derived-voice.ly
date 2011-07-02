\version "2.14.3"
\header {
  texidoc = "For Voice-derived contexts like CueVoice, the lyrics should
still start with the first note."
}

\score {
  \new Staff <<
    \new Voice \relative c'' {
      g2
      <<
          { \voiceOne r2 }
          \new CueVoice = "cue" { \voiceTwo g4 g }
      >>
  }
  \context Lyrics \lyricsto "cue" { A B }
  >>
}
