\version "2.3.8" \header {

    texidoc = "A hack to create feathered
beams: manually make two beams overlap. This uses tuplets to
condense the spacing."

    }
\paper { raggedright = ##t }

\relative \new Staff <<
    \new Voice
    {
	\stemUp
	\once \override Voice.Beam #'positions = #'(0 . 0.5)
	\once \override Voice.TupletBracket #'number-visibility = ##f 
	\times 1/2 { c8[ c c c c c c c]
		 }
    }
    \new Voice {
	\stemUp
	\once \override Voice.Beam #'positions = #'(0 . -0.5) 
	\once \override Voice.TupletBracket #'number-visibility = ##f 
	\times 1/2 { c[ c c c c c c c] }
    }
>>

