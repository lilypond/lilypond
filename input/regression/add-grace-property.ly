\version "2.17.25"

\header {
  texidoc = "@code{\\add-grace-property} can be used at various
context levels in order to override grace properties.  Overrides in
different parallel contexts are independent."
}

<<
 \new Staff = "1" \with { instrumentName =
			  \markup \center-column { Voice mensural } }
   \new Voice \with { $(add-grace-property 'Voice 'NoteHead 'style 'mensural)
		      $(add-grace-property 'Score 'NoteHead 'style 'cross) }
   { \grace d'8 c'1 \bar "|." }

 \new Staff = "2" \with { instrumentName =
			  \markup \center-column { Voice diamond } }
   \new Voice \with { $(add-grace-property 'Voice 'NoteHead 'style 'diamond)   }
   { \grace d'8 c'1 }

 \new Staff = "3" \with { instrumentName =
			  \markup \center-column { Score cross } }
   \new Voice
   { \grace d'8 c'1 }
>>
