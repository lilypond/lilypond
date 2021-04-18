\version "2.23.3"

\header {
  texidoc = "Centered bar numbers may be altered according to
alternatives just like regular bar numbers."
}

\layout {
  \context {
    \Score
    centerBarNumbers = ##t
    alternativeNumberingStyle = #'numbers-with-letters
  }
}

{
  \repeat volta 2 { c'1 1 1 1 }
  \alternative {
    { c'1 }
    { c'1 }
  }
}

