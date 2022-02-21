\version "2.23.7"

\header {
  texidoc = "This exercises @code{GregorianTranscriptionStaff} and
@code{GregorianTranscription} with MIDI output enabled."
}

#(ly:set-option 'warning-as-error #t)

music = \fixed c' {
  \repeat volta 2 { f2 g }
  \repeat volta 3 { a g }
  \repeat volta 4 { f e }
  \repeat volta 1 f1
  g\breve f1
}

words = \lyricmode {
  \repeat volta 2 { Lo2 -- rem. }
  \repeat volta 3 { Ip -- sum. }
  \repeat volta 4 { Do -- lor. }
  \repeat volta 1 Sit.1
  A\breve -- met.1
}

\score {
  <<
    \new GregorianTranscriptionStaff \music
    \new GregorianTranscriptionLyrics \words
  >>

  \midi {}
}
