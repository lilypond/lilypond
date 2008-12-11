\header
{

  texidoc =

  "when @code{tieWaitForNote} is set, the right-tied note does not
 have to follow the lef-tied note directly. When @code{tieWaitForNote}
 is set to false, any tie will erase all pending ties."
  
}

\version "2.11.51"

\paper { ragged-right =  ##t }
\relative {
  c~ e~ g~ <c, e g>
  \set tieWaitForNote = ##t
  c~ e~ g~ <c, e g>

  <c c'> ~

  \set tieWaitForNote = ##f
  <c e> ~
  <c e c'>

}
