
  
breve = \duration { -1 0 }
longa = \duration { -2 0 }

\include "dynamic.ly"
\include "nederlands.ly"		% dutch
\include "script.ly"


Gourlay = 1.0
Wordwrap = 0.0

papersize = "a4"

\include "paper20.ly"

\paper{
	\paper_twenty
}

% ugh
\include "midi.ly"

% declarations for standard directions
left = -1
right = 1
up = 1
down = -1
% zillie spellink?
center=0

break = { \penalty = 10000; }
nobreak = { \penalty = -10000; }

\include "property.ly"

% music = "\melodic\relative c"

