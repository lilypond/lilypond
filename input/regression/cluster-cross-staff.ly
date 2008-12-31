\header {

    texidoc = "Clusters can be written across staves."

}
\layout {
  ragged-right= ##t
}

\version "2.12.0"

\new PianoStaff <<
  \new Staff = "up"     {
    s1 *2
  }
  \new Staff = "down" <<
    \applyMusic #notes-to-clusters \relative c  { <c e>4 <f a> <b e> \change Staff = up <e a>
						  <a d> <d g> }

    { \clef bass s1 * 2 }
  >>
>>
