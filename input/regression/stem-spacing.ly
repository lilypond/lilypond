
\version "2.4.0"

\header{
texidoc="
In a limited number of cases, the optical spacing effects are corrected.  
In this example, space for opposite pointed stems is adjusted.
"

}

\score { 
  \context Voice \relative c {
    
	\time 12/4  c''4 c c c  a f' f, a 
	
  }
  \layout {
    raggedright = ##t
  }  
}

\score { 
  \context Voice \relative c {
    
	\time 12/4  c''4 c c c  a f' f, a 
	
  }
  \layout {
    raggedright = ##t
    \context { \Score
    \override SpacingSpanner #'stem-spacing-correction = #0.0
  }  }
}
 
