\version "1.5.68"

\header{
texidoc="Explicit pitches and durations."
}
\score {
  \notes {
    \pitch #(make-pitch 0 0 0) \duration #(make-duration 1 0)
    \pitch #(make-pitch 1 1 1) \duration #(make-duration 2 0)

  }
}
