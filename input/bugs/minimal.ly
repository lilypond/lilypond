% minimal what?
% dumps core


\score {
  \sequential {\clef bass ; }
  \paper {
%    \translator { \context Score_engraver ; \name "Score";  }
    \translator { \type "Score_engraver"; \name "Score";  }
	linewidth = 30 * \staffspace;
  } 
}
