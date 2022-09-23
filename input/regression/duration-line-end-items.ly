\version "2.23.6"

\header {
  texidoc = "A @code{DurationLine} grob may end with a special behavior.
Currently available are hooks (with settable direction) and arrows."
}

\layout {
  \context {
    \Voice
    \consists "Duration_line_engraver"
    \omit Stem
    \omit Flag
    \omit Beam
    \override NoteHead.duration-log = 2
  }
}

\score {
  \new Voice = "main"
  {
    b1\-
    <<
      \context Voice = "foo" {
        \voiceOne
        d''2\-
      }
      \context Voice = "bar" {
        \voiceTwo
        d'2\-
        \oneVoice
      }
    >>
    << { d''\- } \\ d'\- >>
    e'\-
    \bar "|."
  }
  \layout {
    \context {
      \Voice
      \override DurationLine.bound-details.right.end-style = #'arrow
    }
  }
}

\score {
  \new Voice = "main"
  {
    b1\-
    <<
      \context Voice = "foo" {
        \voiceOne
        \override DurationLine.details.hook-direction = #DOWN
        d''2\-
      }
      \context Voice = "bar" {
        \voiceTwo
        d'2\-
        \oneVoice
      }
    >>
    << { d''\- } \\ d'\- >>
    e'\-
    \bar "|."
  }
  \layout {
    \context {
      \Voice
      \override DurationLine.bound-details.right.end-style = #'hook
    }
  }
}