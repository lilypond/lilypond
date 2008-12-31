\version "2.12.0"

\header {
  lsrtags = "rhythms"
  texidoc = "
By specifying the context, the effect of @code{beatGrouping} can be
limited to the context specified, and the values which may have
been set in higher-level contexts can be overridden.  The
@code{\\set} commands must be placed @emph{after} all @code{\\time}
commands:
"
  doctitle = "Specifying context with beatGrouping"
}

\score {
  \new Staff <<
    \time 7/8
    \new Voice {
      \relative c'' {
        \set Staff.beatGrouping = #'(2 3 2)
        a8 a a a a a a
      }
    }
    \new Voice {
      \relative c' {
        \voiceTwo
        \set beatGrouping = #'(1 3 3)
        f8 f f f f f f
      }
    }
  >>
}
