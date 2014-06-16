\version "2.16.0"

\header {
  texidoc = "Spanners align to musical grobs in paper columns,
ignoring things like pedal marks.

"
}

\score {
  <<
    \new PianoStaff <<
      \new Staff = "up" {
        \clef treble
        \repeat unfold 32 c'4
      }
      \new Dynamics = "dynamics" {
        \repeat unfold 2 {
          s1\cresc s1\f s1\dim s1\p \break
        }
      }
      \new Staff = "down" {
        \clef bass
        \repeat unfold 32 c4
      }
      \new Dynamics= "pedal" {
        \repeat unfold 2 {
          s1\sustainOn s1\sustainOff
        }
      }
    >>
  >>
}
