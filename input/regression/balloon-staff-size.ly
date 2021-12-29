\version "2.23.6"

\header {
  texidoc = "The thickness of balloons scales with staff size."
}

\layout {
  \context {
    \Voice
    \consists Balloon_engraver
    % For visibility
    \override BalloonText.thickness = 2
  }
}

music = {
  \balloonGrobText NoteHead #'(1 . 2) "note head"
  c'1
}

#(set-default-paper-size "a7landscape")

\book {
  \score {
    \music
  }
  \score {
    \music
    \layout {
      #(layout-set-staff-size 50)
    }
  }
}
