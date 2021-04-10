\version "2.23.3"

\header {
  texidoc = "Stem tremolos on cross-staff beams do not cause
circular dependencies."
}

music = \relative c'' {
    f,16:32[ e: d: c: \change Staff = down b: a: g: f:]
}

\new PianoStaff <<
  \new Staff = up \music
  \new Staff = down {
    \clef bass
    s2
  }
>>
