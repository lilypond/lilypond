#(ly:set-option 'old-relative)

\version "2.3.2"
breve = #(ly:make-duration -1 0)
longa = #(ly:make-duration -2 0 )
maxima = #(ly:make-duration -3 0)

\include "music-functions-init.ly"
\include "nederlands.ly"		% dutch
\include "drumpitch-init.ly"		
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

%{

should also set allowBeamBreak, but how to do it "portably"? (ie. also
working with lyric sections)

%}

%% rather name \newline, \newpage ?
break = #(make-event-chord (list (make-penalty-music -10001 0)))
noBreak = #(make-event-chord (list (make-penalty-music 10001 0)))
pageBreak = #(make-event-chord (list (make-penalty-music -10001 -10001)))
pagebreak = \pageBreak % ugh.
noPageBreak = #(make-event-chord (list (make-penalty-music 0 10001)))
noPagebreak = \noPageBreak % ugh

noBeam = #(make-music 'BeamForbidEvent) 
pipeSymbol = #(make-music 'BarCheck)

foo = \notes { \pageBreak }

\include "scale-definitions-init.ly"


melisma = #(make-span-event 'ManualMelismaEvent START)
melismaEnd = #(make-span-event 'ManualMelismaEvent STOP)

\include "grace-init.ly"

% ugh
\include "midi-init.ly"

\include "book-paper-defaults.ly"

\paper {
    mm = #(ly:output-def-lookup $defaultbookpaper  'mm)
    unit = #(ly:output-def-lookup $defaultbookpaper  'unit)

    in = #(* 25.4 mm)
    pt = #(/  in 72.27)
    cm = #(* 10 mm)
    
    \include "engraver-init.ly"
}


#(set-default-paper-size "a4")



%{

%% these would supercede defaults in \bookpaper.
% let's comment this out for now. 


paperEleven = \paper {
    #(paper-set-staff-size (* 11.0 pt))
}

paperThirteen = \paper {
    #(paper-set-staff-size (* 13.0 pt))
}

paperSixteen = \paper {
    #(paper-set-staff-size (* 16.0 pt))
}

paperEightteen = \paper {
    #(paper-set-staff-size (* 18.0 pt))
}

paperTwenty = \paper {
    #(paper-set-staff-size (* 20.0 pt))
}

paperTwentythree = \paper {
    #(paper-set-staff-size (* 23.0 pt))
}

paperTwentysix = \paper {
    #(paper-set-staff-size (* 26.0 pt))
}

\paper { \paperTwenty }

%}

partCombineListener = \paper {
 \context {
	  \Voice
	  \consists Note_heads_engraver
	  \consists Rest_engraver
	  \type "Recording_group_engraver"
	  recordEventSequence = #notice-the-events-for-pc
 }
 \context { \Score skipTypesetting = ##t }
}

#(set-part-combine-listener partCombineListener)

\include "dynamic-scripts-init.ly"
\include "spanners-init.ly"

\include "property-init.ly"

%% reset default duration
unusedEntry = \notes { c4 }

%% must have size argument for GUILE 1.6 compat.
#(define musicQuotes (make-hash-table 29))

%%#(define-public toplevel-music-handler ly:parser-add-book-and-score)
#(define toplevel-book-handler default-toplevel-book-handler)
#(define toplevel-music-handler default-toplevel-music-handler)
#(define toplevel-score-handler default-toplevel-score-handler)

#(define toplevel-score-handler default-toplevel-score-handler)
