
\version "2.3.2"

\header {

    texidoc = "Note grouping events are used to indicate where
analysis brackets start and end.

@cindex bracket
@cindex note groups
@cindex music analysis
@cindex analysis

"
    
}



\score {
    \notes\relative c''
    {
	c4\startGroup\startGroup\startGroup
	c4\stopGroup
	c4\startGroup
	c4\stopGroup\stopGroup
	c4\startGroup
	c4\stopGroup\stopGroup
    }

    \paper {
	\context {
	    \Staff \consists "Horizontal_bracket_engraver"
	}
	raggedright = ##t
    }
}

