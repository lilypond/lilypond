\header {
  texidoc = "Beams do not collide with clefs, key signatures, time
  signatures"
}

\layout {
  ragged-right = ##t
%  debug-beam-scoring = ##t
}

\version "2.19.21"

\relative {
  \time 2/4
  c'8[ \clef "bass" e,, ]
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
