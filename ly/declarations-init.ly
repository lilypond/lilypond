\version "2.12.0"

%% < 1.8 compatibility switch
#(ly:set-option 'old-relative)

%% named durations
breve = #(ly:make-duration -1 0)
longa = #(ly:make-duration -2 0)
maxima = #(ly:make-duration -3 0)

\include "markup-init.ly"
\include "music-functions-init.ly"
\include "toc-init.ly"

%% default note names are dutch
\include "nederlands.ly"

\include "drumpitch-init.ly"		
\include "chord-modifiers-init.ly"
\include "script-init.ly"

\include "chord-repetition-init.ly"

% declarations for standard directions
left = #-1
right = #1
up = #1
down = #-1
start = #-1
stop = #1
smaller = #-1
bigger = #1
center = #0

%% FIXME
%% should also set \override Beam #'breakable, but how to do it "portably"? (ie. also
%% working with lyric sections)
%%
%% try \once \override Score.Beam #'breakable = ##t

%% rather name \newline, \pageBreak ?
break = #(make-event-chord (list (make-music 'LineBreakEvent 'break-permission 'force)))
noBreak = #(make-event-chord (list (make-music 'LineBreakEvent 'break-permission '())))
%% \pageBreak, \noPageBreak, \pageTurn, \noPageTurn, \allowPageTurn are defined
%% as music functions

stopStaff = #(make-event-chord (list (make-span-event 'StaffSpanEvent STOP)))
startStaff = #(make-event-chord (list (make-span-event 'StaffSpanEvent START)))


%
% Code articulation definitions
%
noBeam = #(make-music 'BeamForbidEvent) 
pipeSymbol = #(make-music 'BarCheck)
bracketOpenSymbol = #(make-span-event 'BeamEvent START)
bracketCloseSymbol = #(make-span-event 'BeamEvent STOP)
tildeSymbol = #(make-music 'TieEvent)
parenthesisOpenSymbol =  #(make-span-event 'SlurEvent START)
parenthesisCloseSymbol = #(make-span-event 'SlurEvent STOP)
escapedExclamationSymbol = #(make-span-event 'CrescendoEvent STOP)
escapedParenthesisOpenSymbol = #(make-span-event 'PhrasingSlurEvent START)
escapedParenthesisCloseSymbol = #(make-span-event 'PhrasingSlurEvent STOP)
escapedBiggerSymbol = #(make-span-event 'DecrescendoEvent START)
escapedSmallerSymbol = #(make-span-event 'CrescendoEvent START)


#(define fretboard-table (make-hash-table 100))
#(define chord-shape-table (make-hash-table 100))

\include "scale-definitions-init.ly"

melisma = #(context-spec-music (make-property-set 'melismaBusy #t) 'Bottom)
melismaEnd = #(context-spec-music (make-property-unset 'melismaBusy) 'Bottom)

laissezVibrer = #(make-music 'LaissezVibrerEvent)
repeatTie = #(make-music 'RepeatTieEvent)
		  
\include "grace-init.ly"
\include "midi-init.ly"
\include "paper-defaults-init.ly"

\layout {
    mm = #(ly:output-def-lookup $defaultpaper 'mm)
    unit = #(ly:output-def-lookup $defaultpaper 'unit)

    in = #(* 25.4 mm)
    pt = #(/  in 72.27)
    cm = #(* 10 mm)
    
    \include "engraver-init.ly"

    #(set-paper-dimension-variables (current-module))
}

#(set-default-paper-size (ly:get-option 'paper-size))

partCombineListener = \layout {
    \context {
	\Score
	skipTypesetting = ##t
	ignoreBarChecks = ##t
	\alias "Timing"
    }
}

\include "dynamic-scripts-init.ly"
\include "spanners-init.ly"
\include "property-init.ly"

setDefaultDurationToQuarter = { c4 }

%% MAKE-HASH-TABLE in GUILE 1.6 takes mandatory size parameter.
#(define musicQuotes (make-hash-table 29))

#(define toplevel-book-handler print-book-with-defaults)
#(define toplevel-bookpart-handler collect-bookpart-for-book)
#(define toplevel-music-handler collect-music-for-book)
#(define toplevel-score-handler collect-scores-for-book)
#(define toplevel-text-handler collect-scores-for-book)

#(define book-bookpart-handler ly:book-add-bookpart!)
#(define book-music-handler collect-book-music-for-book)
#(define book-score-handler ly:book-add-score!)
#(define book-text-handler ly:book-add-score!)

#(define bookpart-score-handler ly:book-add-score!)
#(define bookpart-text-handler ly:book-add-score!)
#(define bookpart-music-handler collect-book-music-for-book)

\include "predefined-fretboards-init.ly"
