
\version "2.3.16"
\header {
texidoc = "
 When tightly spaced, the spaces between elements (hinterfleisch?) 
 may approach zero. In that case, 
 stems may touch the bar lines and opposite stems may touch eachother.
 In these situations,
 a mininum of about a note-width/interline space is needed,
 so that all vertical lines are approximately equally spaced in tightly 
 spaced music.

 "
 
 }
\score {
	 \relative c''{ 
		r1 e4 f, e' f,
	}
	\paper { 
		linewidth = 25.0 \mm
		indent = 0.0\mm
	}
}

