\version "2.13.9"
\include "predefined-ukulele-fretboards.ly"


mychords = \chordmode {
  c1 c:m c:aug c:dim c:7 c:maj7 c:m7
  c:6 c:sus2 c:sus4 c:9
  \break
}

chordsline = {
  \mychords
  \transpose c cis {
    \mychords
  }
  \transpose c des {
    \mychords
  }
  \transpose c d {
    \mychords
  }
  \transpose c dis {
    \mychords
  }
 \transpose c ees {
    \mychords
  }
  \transpose c e {
    \mychords
  }
  \transpose c f {
    \mychords
  }
  \transpose c fis {
    \mychords
  }
  \transpose c ges {
    \mychords
  }
  \transpose c g {
    \mychords
  }
  \transpose c gis {
    \mychords
  }
  \transpose c aes {
    \mychords
  }
  \transpose c a {
    \mychords
  }
  \transpose c ais {
    \mychords
  }
  \transpose c bes {
    \mychords
  }
  \transpose c b {
    \mychords
  }
}

<<
  \context ChordNames {
    \chordsline
  }
  \context FretBoards {
    \set stringTunings = #ukulele-tuning
    \chordsline
  }
  \context Staff {
    \chordsline
  }
>>
