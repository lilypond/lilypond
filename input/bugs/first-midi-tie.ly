\header {

texidoc="

    Interpreting music...
    m.ly:3:14: warning: No ties were created!:
       <c4 g' e'>~
                  <c g' e'>

    MIDI output to `m.midi'...
"
}

chordexamples = \notes \relative c' {
  <c4 g' e'>~<c g' e'>
  <c4 g' e'>~<cis4 gis' e'>
  <c4 g' e'>~<\context Voice = x {<cis4 gis'>} e'>
  <c e g>~<a cis e g bes>
}

\score {
  \context PianoStaff <
    \context Staff = upper <
      \clef treble
      \context Voice = i { \voiceOne \chordexamples }
    >
  >
  \paper {  }
  \midi { \tempo 4 = 120 }
}

