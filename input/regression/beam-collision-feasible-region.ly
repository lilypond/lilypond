\version "2.16.0"

\header {
  texidoc = "A rough guess for collisions is taken into account when
  choosing initial beam configurations; the initial position may be
  chosen to be either above or below large collisions."
}

\layout {
  ragged-right = ##t 
}

partOne = <<
  { s4 s8 \key f \major s8 }
  {
    g8[ c'''8]
    g8[ c'''8]
  }
>>

partTwo = <<
  { \clef bass \key c \major s4 s8 \key f \major s8 }
  {
    c,8[ e'8]
    c,8[ e'8]
  }
>>

partThree = <<
  { \clef bass \key c \major s4 s8 \key f \major s8 }
  {
    b,,8[ e'8]
    b,,8[ e'8]
  }
>>

\new Voice {
  \partOne
  \partTwo
  \partThree
}

