\version "2.1.36"
\header {

    texidoc = "Unfolding tremolo repeats. Both fragments fill one 2/4
 measure with 32nd notes exactly."

}


\score{
    \notes{
	\time 2/4 
	\apply #unfold-repeats 	
	{ \repeat tremolo 8 { c'32 e' } |
	  \repeat tremolo 16 c'32 } | \bar "|."
    }
    \paper { raggedright = ##t }  
}
