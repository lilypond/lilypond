\header {
texidoc = "Tuplet bracket formatting supports numerous options:
NB should have no bracket, B should have bracket.
"
}
 

\version "1.5.68"


\score { 
    \context Voice \notes\relative c'' {

 	\times 2/3 { c'8 c,, c }
 	\times 2/3 { c'8 c'' c,, }

	
 	\times 2/3 { [c8^"NB" c c]  }
	
 	\times 2/3 { c8^"B" [c c]  }
 	\times 2/4 { r8_"B" [c, c'] r8 }
	
 	\property Voice.TupletBracket \override #'bracket-visibility = #'if-no-beam  
 	\times 2/3 { [c8 c c]  }
	
	\property Voice.TupletBracket \override #'direction = #1
 	\property Voice.TupletBracket \override #'number-visibility = ##f
 	\times 2/3 { c8^""^""^"up, no digit" [c c]  }
 	\property Voice.TupletBracket \revert #'number-visibility

	\property Voice.TupletBracket \override #'bracket-visibility = ##t
	\property Voice.TupletBracket \override #'edge-height = #'(0.0 . 0.0)
	\property Voice.TupletBracket \override #'shorten-pair = #'(2.0 . 2.0)
	\times 4/6 { c_"shorter, no edges" f b  b f c}	
	\property Voice.TupletBracket \revert #'edge-height
	\property Voice.TupletBracket \revert #'shorten-pair
    	\property Voice.TupletBracket \override #'edge-widen = #'(-0.5 . 0.5)
	\times 2/3 { b^""^""^"angled edges" b b }
 	\property Voice.TupletBracket \revert #'direction
	\times 2/3 { b b b }

    }
}
