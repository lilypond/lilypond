\version "2.12.0"

\header {
  lsrtags = "rhythms"
  texidoc = "
The layout of grace expressions can be changed throughout the
music using the functions @code{add-grace-property} and
@code{remove-grace-property}.  The following example undefines
the @code{Stem} direction for this grace, so that stems do not
always point up, and changes the default note heads to crosses.
"
  doctitle = "Tweaking grace layout within music"
}

\relative c'' {
  \new Staff {
    #(remove-grace-property 'Voice 'Stem 'direction)
    #(add-grace-property 'Voice 'NoteHead 'style 'cross)
    \new Voice {
       \acciaccatura { f16 } g4
       \grace { d16[ e] } f4
       \appoggiatura { f,32[ g a] } e2
    }
  }
}
