\version "1.7.18"

\header{
texidoc="
In a limited number of cases, LilyPond corrects for optical spacing
effects.  In this example, space for opposite pointed stems is adjuste
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
    SpacingSpanner \override #'stem-spacing-correction = #0.0
  }  }
}
 %% new-chords-done %%
