\version "2.19.21"
\header {
  texidoc = "For Voice-derived contexts like CueVoice, the lyrics should
still start with the first note."
}

\score {
  \new Staff <<
    \new Voice \relative {
      g'2 
      <<
          { \voiceOne r2 }
          \new CueVoice = "cue" { \voiceTwo g4 g }
      >>
  }
  \context Lyrics \lyricsto "cue" { A B }
  >>
}
