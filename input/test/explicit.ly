\version "1.7.3"

\header{
texidoc="Explicit pitches and durations."
}
\score {
  \notes {
    \pitch #(ly:make-pitch 0 0 0) \duration #(ly:make-duration 1 0)
    \pitch #(ly:make-pitch 1 1 1) \duration #(ly:make-duration 2 0)

  }
}
