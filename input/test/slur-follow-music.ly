\version "1.5.68"

\score { 
  \context Voice \notes\relative c {
    % CASE 3
	d''4 ( ) b a ( ) e' \break
	g,( \stemDown ) b \stemBoth c ( ) f, \break
	
	% no adjusting...
	\stemUp [d'8 ( b a] \stemBoth ) e' \break
	\stemUp [g,8 ( b d] \stemBoth ) c \break
	
	% still ugly
	g4 ( b d ) c \break
	
	%TIES
	d ~ b a ~ e' \break
	g, ~ \stemDown b \stemBoth c ~ f, \break
	
	
  }
  \paper {
    linewidth=-1.0
  }  
  \midi { }
}
