\version "2.23.0"
\header {
  texidoc = "A single @code{\\alternative} is a way to indicate a
repeat count when there is no variation."
}

\paper { ragged-right = ##t }

\new Score {
  \partial 8 r8 \repeat volta 2 f'1 \alternative { a'1 }
}
