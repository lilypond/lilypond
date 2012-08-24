\header {

  texidoc = "Notes can be set in the Walker (Christian Harmony) style."

}
\version "2.16.0"

\relative c' {
  \key c \major
  \walkerHeads
  \stemDown
  c1 d e f g a b c d e f g a b c
  c,,2 d e f g a b c d e f g a b c
  c,,4 d e f g a b c d e f g a b c
  \stemUp % We need to manually raise the stem for low flagged notes
  c,,8 d e f g a
  \stemDown % Put it back down for the higher notes
  b c d e f g a b
}

