
\version "1.5.68"
breve = \duration #(make-duration -1 0)
longa = \duration #(make-duration -2 0 )
maxima = \duration #(make-duration -3 0)

\include "nederlands.ly"		% dutch
\include "chord-modifiers-init.ly"
\include "script-init.ly"

% declarations for standard directions
left = -1
right = 1
up = 1
down = -1
start = -1
stop = 1
smaller = -1
bigger = 1
center=0
	
break = #(make-event-chord (list (make-penalty-music -10001)))
noBreak =  #(make-event-chord (list (make-penalty-music 10001)))

\include "scale-definitions-init.ly"

melisma = \property Staff.melismaBusy = ##t
melismaEnd = \property Staff.melismaBusy = ##f


\include "engraver-init.ly"
\include "grace-init.ly"

% ugh
\include "midi-init.ly"

papersize = "a4"
paperfile = \papersize + "-init.ly"

\include "generic-paper-init.ly"
\include "paper20-init.ly"


\include "dynamic-scripts-init.ly"
\include "spanners-init.ly"

\include "property-init.ly"



% reset default duration
unusedEntry = \notes { c4 }	

% music = "\melodic\relative c"

