#(ly:set-point-and-click 'line-column)
\version "2.3.16"

\header {

texidoc = "Stress optimal page breaking.  This should look
    nice on 4 a6 pages. "

    
    copyright = "Copyright by /me"
    
    title = "Title"
    subtitle = "(and (the) subtitle)"
    subsubtitle = "Sub sub title"
    poet = "Poet"
    composer = "Composer"
    texttranslator = "Text Translator"
    opus = "opus 0"
    meter = "Meter (huh?)"
    arranger = "Arranger"
    instrument = "Instrument"
    piece = "Piece"
}

#(set-default-paper-size "a6" 'portrait)


pattern =  { a b c d \break }
\book {    
    \score {
	\context Staff  \relative c' {
	    %% 16: ideally cramped
	    %% 17: very bad without density
				%	\repeat unfold 17 { a b c d \break }

	    \pattern
	    \pattern
				% \noPageBreak
	    \pattern
	    
				% the following changes the location of the break.
				% 
				% \pageBreak
	    
	    \pattern
	    \pattern
	    \pattern
	    \pattern
	    \repeat unfold 10 \pattern
	}

    }
    
}
