
\header {

 texidoc = "setting the @code{suggestAccidentals} will print
accidentals vertically relative to the note.  This is useful for
denoting Musica Ficta."

}

\version "2.21.0"
\paper {
  ragged-right = ##t
}

\relative {
  \time 3/4
  \set suggestAccidentals = ##t
  cis''^> gis'-! c?
  \set suggestAccidentals = #'cautionary
  cis, gis' c?

  \override AccidentalSuggestion.parenthesized = ##t
  cis, gis' c?
}
