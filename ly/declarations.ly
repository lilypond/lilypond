
  
breve = \duration #'( -1 0)
longa = \duration #'( -2 0 )
maxima = \duration #'( -3 0 )

#(eval-string (ly-gulp-file "generic-property.scm"))

% urg, move to basic property?
#(eval-string (ly-gulp-file "slur.scm"))

\include "nederlands.ly"		% dutch
\include "chord-modifiers.ly"
\include "script.ly"


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

break =  \penalty  -1000000; 
nobreak =  \penalty 1000000; 
\include "scale-definitions.ly"

melisma = \property Staff.melismaBusy = ##t
melismaEnd = \property Staff.melismaBusy = ##f



papersize = "a4"
\include "generic-paper.ly"
\include "paper20.ly"

% ugh
\include "midi.ly"

\include "textscripts.ly"
\include "spanners.ly"

\include "property.ly"



unusedEntry = \notes { c4 }		% reset default duration

% music = "\melodic\relative c"

