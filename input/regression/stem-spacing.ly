
\version "2.1.23"

\header{
texidoc="
In a limited number of cases, LilyPond corrects for optical spacing
effects.  In this example, space for opposite pointed stems is adjusted.
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
    \translator { \ScoreContext
    \override SpacingSpanner #'stem-spacing-correction = #0.0
  }  }
}
 
