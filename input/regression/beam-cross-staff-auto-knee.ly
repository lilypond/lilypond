\version "1.5.68"

\header{

    texidoc="Automatic cross-staff knees also work (here we see them
with explicit staff switches)."

     }

\score {
  \notes \context PianoStaff <
    \context Staff = "up" \notes\relative c''{
      [ b8 \translator Staff="down" d,, ]
      [ c \translator Staff="up" c'' ]
      [ b, \translator Staff="down" d^"no knee" ]
    }
    \context Staff = "down" {
      \clef bass 
      s2.
    }
  >
  \paper{
    linewidth = -1.0
  }
}
