
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UGH UGH UGH UGHUGH FIXME
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -> duplication!

breve = \duration #'( -1 0)
longa = \duration #'( -2 0 )
maxima = \duration #'( -3 0 )

#(begin
  (eval-string (ly-gulp-file "slur.scm"))
  (eval-string (ly-gulp-file "generic-property.scm"))
  (eval-string (ly-gulp-file "basic-properties.scm"))
 )

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

%papersize = "a4"
%\include "generic-paper.ly"

#(eval-string (ly-gulp-file "paper.scm"))

\paper {
	texsetting = "";
	pssetting = "";
	scmsetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse;\n";% UGH. 
}

\include "paper-as9.ly"

% ugh
\include "midi.ly"

\include "textscripts.ly"
\include "spanners.ly"

\include "property.ly"



unusedEntry = \notes { c4 }		% reset default duration

% music = "\melodic\relative c"

