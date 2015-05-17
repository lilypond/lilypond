\version "2.19.21"

\header {
  texidoc = "The @code{X-offset} of @code{DynamicText} grobs in a
@code{Dynamics} context should be averaged over the center of
@code{NoteColumn} grobs in the @code{DynamicText}'s @code{PaperColumn}.
"
}

\score {
  <<
    \new PianoStaff <<
      \new Staff = "up" {
        \clef treble
        \relative {
          c'4\p c c\mp c |
          c4\mf c c\f c |
          <<
            \repeat unfold 8 c4
            \new Dynamics = "dynamics" \with {
              alignBelowContext = "up"
            } {
              s4\p s s\mp s |
              s4\mf s s\f s
            }
          >>
        }
      }
      \new Staff = "down" {
        \clef bass
        \repeat unfold 16 c4
      }
    >>
  >>
}
