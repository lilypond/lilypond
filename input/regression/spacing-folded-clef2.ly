\version "2.19.21"
\header {

texidoc = "A clef can be folded below notes in a different staff, if
there is space enough. With @code{Paper_column} stencil callbacks we
can show where columns are in the score."
}

\layout {
    ragged-right = ##t

    \context {
	\Score
	\override NonMusicalPaperColumn.stencil = #ly:paper-column::print
	\override PaperColumn.stencil = #ly:paper-column::print	  
	\override NonMusicalPaperColumn.font-family = #'roman
	\override PaperColumn.font-family = #'roman	  

    }
}
		   
\relative <<
    \new Staff  { c''4 c4 c4 c \bar "|." }
    \new Staff { \clef bass c,2 \clef treble  c'2 }
>>

