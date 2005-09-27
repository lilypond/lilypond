\header {

    texidoc = "Clusters can be written across staves."

}
\layout {
  raggedright= ##t
}

\version "2.7.10"

\new PianoStaff <<
  \context Staff = "up"     {
    s1 *2
  }
  \context Staff = "down" <<
    \applyMusic #notes-to-clusters \relative c  { <c e>4 <f a> <b e> \change Staff = up <e a>
						  <a d> <d g> }

    { \clef bass s1 * 2 }
  >>
>>
