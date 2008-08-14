\version "2.11.51"

\header {

  texidoc = "At the beginning of a system, the @code{|:} repeat
barline is drawn between the staves, but the @code{:|} is not."

}

\layout{ ragged-right = ##t }


\new PianoStaff <<
  \new Staff = "up" {
    \bar "|:" r1
    \bar ":|" \break r1
  }
  \new Staff = "down" { r r }
>>


