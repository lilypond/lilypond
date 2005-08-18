
\header {

  texidoc = "The flags of 8th notes take some space, but not
    too much: the space following a flag is less than the space
    following a beamed 8th head."

}

\relative c'' \context Staff {
  \set autoBeaming = ##f
  a8[ a8 a8 a8]
  a8 a8 a8 a8
}
\layout {
  raggedright = ##t
}


\version "2.6.0"

