\version "2.3.2"

\header {

    texidoc = "Hyphens are print at the beginning of the line only when
they go past the first note. "

    }

\score {
<<    \notes \new Staff \relative c'' { \time 1/4 c16[ c c  c]
\time 1/4
c16[ c c c]
\time 1/4
r c16[ c c]

}
     \new Lyrics \lyrics {
	bla16 -- bla -- bla -- bla --
	bla -- bla -- bla -- bla8 --
	       bla16 -- bla -- bla 
       }>>
    \paper   {
	indent = 0.0 \cm
	linewidth = 3.4 \cm

	\context {
	    \Staff \remove "Time_signature_engraver"
	}
	
    }
      
}

	
