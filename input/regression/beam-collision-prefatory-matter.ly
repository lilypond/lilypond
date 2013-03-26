\header {
  texinfo = "Beams do not collide with clefs, key signatures, time
  signatures"
}

\layout {
  ragged-right = ##t
%  debug-beam-scoring = ##t
}

\version "2.17.15"

\relative c' {
  \time 2/4
  c8[ \clef "bass" e,, ]
  r8
  e8[ |
      \time 1/4
    e]
  e[
  e] r8
  \time 4/4

  e[
  \key f \major
  e]
  e[
  \key cis \major
  e]
  
}
