\header {
  texidoc = "We use a skyline algorithm to determine the distance to the next
system instead of relying only on bounding boxes. This keeps gaps between
systems more uniform."
}

\paper {ragged-right = ##t}
#(set-default-paper-size "a6")

\version "2.16.0"
\book {
  \score {
    {
      a,,1 | a'4 b' c'' d'' \break
      \repeat unfold 2 {a' b' c'' d''} | b''''1
    }
  }
}
