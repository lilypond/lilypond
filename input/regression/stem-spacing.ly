
\version "2.1.36"

\header{
texidoc="
In a limited number of cases, the optical spacing effects are corrected.  
In this example, space for opposite pointed stems is adjusted.
"

}

\score { 
  \context Voice \notes\relative c {
    
	\time 12/4  c''4 c c c  a f' f, a 
	
  }
  \paper {
    raggedright = ##t
  }  
}

\score { 
  \context Voice \notes\relative c {
    
	\time 12/4  c''4 c c c  a f' f, a 
	
  }
  \paper {
    raggedright = ##t
    \context { \ScoreContext
    \override SpacingSpanner #'stem-spacing-correction = #0.0
  }  }
}
 
