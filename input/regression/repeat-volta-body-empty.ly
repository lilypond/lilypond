\version "2.21.6"

\header{
  texidoc="If the body of a volta repeat is empty, the alternatives are
still rendered with the expected volta notation."
}

body = { }

\new Score {
  \repeat volta 2 \body | R1
}

\new Score {
  R1 | \repeat volta 2 \body
}

\new Score {
  R1 | \repeat volta 2 \body | R1
}

\new Score {
  \repeat volta 2 \body \alternative { a'1 b' } | R1
}

\new Score {
  R1 | \repeat volta 2 \body \alternative { a'1 b' }
}

\new Score {
  R1 | \repeat volta 2 \body \alternative { a'1 b' } | R1
}
