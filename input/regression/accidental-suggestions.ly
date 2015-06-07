
\header {

 texidoc = "setting the @code{suggestAccidentals} will print
accidentals vertically relative to the note.  This is useful for
denoting Musica Ficta."
 
}

\version "2.19.21"
\paper {
  ragged-right = ##t
}

\relative {
  \time 2/4 
  \set suggestAccidentals = ##t 
  cis''^> gis'-!
  \override AccidentalSuggestion.parenthesized = ##t
  cis,_"paren" gis'

}
  
  
