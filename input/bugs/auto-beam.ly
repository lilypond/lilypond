\version "1.3.148"

\header {
texidoc="The Automatic beamer does not put @strong{unfinished} beams
on the last notes of a score."
}

\score {
  <
    \context Staff=a\notes\relative c'' {
      a8 a8 a2. a8 a8
    }
    \notes\relative c'' {
      a8 a a a a2
      a8 a a a
    }
  >
}
