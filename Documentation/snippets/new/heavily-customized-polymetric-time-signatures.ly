\version "2.17.15"

\header {
  lsrtags = "contemporary-notation, percussion, real-music, rhythms"

  texidoc = "
Though the polymetric time signature shown was not the most essential
item here, it has been included to show the beat of this piece (which
is the template of a real Balkan song!).

"
  doctitle = "Heavily customized polymetric time signatures"
}

melody = \relative c'' {
  \set Staff.instrumentName = #"Bb Sop."
  \key g \major
  \compoundMeter #'((3 8) (2 8) (2 8) (3 8) (2 8) (2 8)
                    (2 8) (2 8) (3 8) (2 8) (2 8))
  c8 c c d4 c8 c b c b a4 g fis8 e d c b' c d e4-^ fis8 g \break
  c,4. d4 c4 d4. c4 d c2 d4. e4-^ d4
  c4. d4 c4 d4. c4 d c2 d4. e4-^ d4 \break
  c4. d4 c4 d4. c4 d c2 d4. e4-^ d4
  c4. d4 c4 d4. c4 d c2 d4. e4-^ d4 \break
}

drum = \new DrumStaff \drummode {
  \bar ".|:" bd4.^\markup { Drums } sn4 bd \bar ";" sn4.
  bd4 sn \bar ";" bd sn bd4. sn4 bd \bar ":|."
}

{
  \melody
  \drum
}
