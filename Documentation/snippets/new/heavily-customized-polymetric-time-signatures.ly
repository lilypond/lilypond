\version "2.25.33"

\header {
  categories = "Contemporary notation, Percussion, Real music, Rhythms"

  texidoc = "
Though the polymetric time signature shown is not the most essential
item here, it has been included to show the beat of this piece (which
is the template of a real Balkan song, by the way).
"

  doctitle = "Heavily customized polymetric time signatures"
}


melody = \relative c'' {
  \key g \major
  \timeAbbrev #'((3 8) (2 8) (2 8) (3 8) (2 8) (2 8)
                 (2 8) (2 8) (3 8) (2 8) (2 8))
  \set Timing.beamExceptions = #'()
  \set Timing.beatStructure = 3,2,2,3,2,2,2,2,3,2,2
  c8 c c d4 c8 c b c b a4 g fis8 e d c b' c d e4-^ fis8 g \break
  c,4. d4 c4 d4. c4 d c2 d4. e4-^ d4
  c4. d4 c4 d4. c4 d c2 d4. e4-^ d4 \break
}

drum = \new DrumStaff \drummode {
  \repeat volta 2 {
    bd4.^\markup { Drums } sn4 bd \bar ";"
    sn4. bd4 sn \bar ";"
    bd sn bd4. sn4 bd
  }
}

\new Staff {
  \melody
  \drum
}
