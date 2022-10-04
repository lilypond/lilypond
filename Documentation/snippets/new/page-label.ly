\version "2.23.14"

\header {
  lsrtags = "spacing"

  texidoc = "
Page labels may be placed inside music or at top-level, and referred to
in markups.
"

  doctitle = "Page label"
}


\label license
\markup "This snippet is available under the Creative Commons
Public Domain Dedication license."

{
  \repeat volta 2 {
    \label startRepeat
    \repeat unfold 20 { c'2 2 }
    \pageBreak
    2 2
  }
  \textEndMark \markup {
    \with-link #'startRepeat \line {
      To page \page-ref #'startRepeat "0" "?"
    }
  }
}

\markup {
  See page \page-ref #'license "0" "?" for
  licensing information.
}
