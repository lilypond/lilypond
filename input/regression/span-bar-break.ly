\version "2.7.32"

\header {

  texidoc = "At the beginning of a system, the @code{|:} repeat
barline is drawn between the staves, but the @code{:|} is not."

}

\layout{ ragged-right = ##t }


\new PianoStaff <<
  \context Staff = "up" {
    \bar "|:" r1
    \bar ":|" \break r1
  }
  \context Staff = "down" { r r }
>>


