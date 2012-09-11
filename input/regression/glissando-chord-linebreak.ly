\version "2.16.0"

\header {

  texidoc = "
A glissando between chords should not interfere with line breaks.  In
this case, the music should be in two lines and there should be no
warning messages issued.  Also, the glissando should be printed.
"

}

theNotes = {
  <c e>4 <c e> <c e>
  \glissando
  d
}

\score {
  \new Staff {
    \relative c'' {
      \theNotes
      \break
      \theNotes
    }
  }
}
