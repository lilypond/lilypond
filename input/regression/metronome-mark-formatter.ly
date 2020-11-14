\version "2.17.27"

\header {
  texidoc = "@code{metronomeMarkFormatter} supports
all note head styles and flags styles.  Setting @code{font-name} for
@code{MetronomeMark} does not disturb the glyphs for note-head and flag.
"
}

\paper {
  ragged-right = ##f
  indent = 30
}

%{
You may have to install additional fonts.

Red Hat Fedora

  dejavu-fonts-all

Debian GNU/Linux, Ubuntu

  fonts-dejavu-core
%}

mus =
\relative c'' {
  \tempo "Allegro" 8=120-140
  a8 b c d a b c d

  \override Score.MetronomeMark.font-name = "DejaVu Sans Mono"
  \tempo "Allegro" 8=140
  a b c d a b c d
}

%% Some Examples:
\score {
  \new Staff
  \with {
    instrumentName = "default"
  }
  \mus

  \layout {}
}

\score {
  \new Staff
  \with {
    instrumentName =
      \markup \column { "default-note-head"
                        "old-straight-flag" }
  }
  \mus

  \layout {
    \context {
      \Score
      \override MetronomeMark.flag-style = #'old-straight-flag
    }
  }
}

\score {
  \new Staff
  \with {
    instrumentName =
      \markup \column { "default-note-head"
                        "modern-straight-flag" }
  }
  \mus

  \layout {
    \context {
      \Score
      \override MetronomeMark.flag-style = #'modern-straight-flag
    }
  }
}

\score {
  \new Staff
  \with {
    instrumentName =
      \markup \column { "default-note-head"
                        "flat-flag" }
  }
  \mus

  \layout {
    \context {
      \Score
      \override MetronomeMark.flag-style = #'flat-flag
    }
  }
}

\score {
  \new Staff
  \with {
    instrumentName =
      \markup \column { "diamond-note-head"
                        "modern-straight-flag" }
  }
  \mus

  \layout {
    \context {
      \Score
      \override MetronomeMark.style = #'diamond
      \override MetronomeMark.flag-style = #'modern-straight-flag
    }
  }
}

\score {
  \new MensuralStaff
  \with {
    instrumentName =
      \markup \column { "mensural-note-head"
                        "mensural-flag" }
  }
  \mus

  \layout {
    \context {
      \Score
      \override MetronomeMark.style = #'mensural
      \override MetronomeMark.flag-style = #'mensural
    }
  }
}
