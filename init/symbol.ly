% symbol.ly
  
\version "0.1.5";
  
breve = \duration { -1 0 }
longa = \duration { -2 0 }

\include "dynamic.ly"
\include "dutch.ly" 
\include "script.ly"


Gourlay = 1.0
Wordwrap = 0.0

\include "paper16.ly"
\include "paper20.ly"


default_paper = \paper{
	\paper_sixteen
}

% ugh
\include "midi.ly"

% declarations for standard directions
left = -1
right = 1
up = 1
down = -1
center=0


stemup = {
	\skip 1*0;
	% Stupid hack to make < { \stemup } > work
%	\property Thread.cocktailbar = 1
	\property Voice.ydirection = \up 
	}
stemboth= {
	\skip 1*0;
	% \property Thread.cocktailbar = 0
	\property Voice.ydirection = \center
}
stemdown = { 	
	\skip 1*0;
	%\property Thread.cocktailbar = -1
	\property Voice.ydirection = \down
}


