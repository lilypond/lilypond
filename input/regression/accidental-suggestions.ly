
\header {

 texidoc = "setting the @code{suggestAccidentals} will print
accidentals vertically relative to the note.  This is useful for
denoting Musica Ficta."
 
}

\version "2.7.32"
\paper {
  ragged-right = ##t
}

\relative c'' {
  \time 2/4 
  \set suggestAccidentals = ##t 
  cis^> gis'-|
  \override AccidentalSuggestion #'cautionary-style = #'parentheses
  cis,_"paren" gis'
  \override AccidentalSuggestion #'cautionary-style = #'()
  cis,_"no caut style"  gis'

}
  
  
