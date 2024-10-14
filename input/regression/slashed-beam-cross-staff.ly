\version "2.25.21"

\header {
  texidoc = "Slashed beams work with cross-staff beams."
}

music = <<
  \new Staff = "1" {
    \slashedGrace {
      \stemDown
      a'''16[
      \change Staff = "2"
      \stemUp
      bes,
      \change Staff = "1"
      \stemDown
      fis''16
      \change Staff = "2"
      \stemUp
      g32]
    }
    \change Staff = "1"
    \oneVoice
    es'2 r
    \slashedGrace {
      \stemDown
      a'''16[
      \change Staff = "2"
      \stemUp
      bes,
      \change Staff = "1"
      \stemDown
      fis''16
      \change Staff = "2"
      \stemUp
      g32
      \change Staff = "1"
      \stemDown
      ais'64]
    }
    \change Staff = "1"
    \oneVoice
    es'2 r
    s1
  }

  \new Staff = "2" {
    \clef bass
    \grace s4
    s1*2
    \slashedGrace {
      \stemUp
      b16[
      \change Staff = "1"
      \stemDown
      a'''
      \change Staff = "2"
      \stemUp
      bes,
      \change Staff = "1"
      \stemDown
      fis''16
      \change Staff = "2"
      \stemUp
      g32
      \change Staff = "1"
      \stemDown
      ais'64
      ]
    }
    \oneVoice
    es'2 r
    \bar "|."
  }
>>

\new PianoStaff \music

\new PianoStaff \with { \override Beam.details.slash-side = #RIGHT } \music
