\version "2.19.21"

\header {

  texidoc = "The flags of 8th notes take some space, but not
    too much: the space following a flag is less than the space
    following a beamed 8th head."

}

\layout {
  ragged-right = ##t
}

\relative \context Staff {
  \set autoBeaming = ##f
  a'8[ a8 a8 a8]
  a8 a8 a8 a8
}




