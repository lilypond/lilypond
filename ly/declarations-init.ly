#(ly:set-option 'old-relative)

\version "2.1.36"
breve = #(ly:make-duration -1 0)
longa = #(ly:make-duration -2 0 )
maxima = #(ly:make-duration -3 0)

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
pagebreak = #(make-event-chord (list (make-penalty-music -10001 -10001)))
noPagebreak = #(make-event-chord (list (make-penalty-music 0 10001)))

noBeam = #(make-music 'BeamForbidEvent) 
pipeSymbol = #(make-music 'BarCheck)

\include "scale-definitions-init.ly"

melisma = #(make-span-event 'ManualMelismaEvent START)
melismaEnd = #(make-span-event 'ManualMelismaEvent STOP)

\include "grace-init.ly"

% ugh
\include "midi-init.ly"


% Do units first; must be done before any units are specified.
\paper {
    unit = #(ly:unit)
    mm = 1.0
    in = 25.4
    pt = #(/  in 72.27)
    cm = #(* 10 mm)

    inputencoding = #"TeX"
    raggedright = ##f
    raggedlast = ##f 
    packed = ##f
    
    #(define $is-paper #t)
    
    #(define font-defaults
      '((font-encoding . fetaMusic))
      )

    #(define text-font-defaults
      '((font-encoding . latin1)
	(baseline-skip . 2)
	(word-space . 0.6)
	))

    #(define page-breaking ly:ragged-page-breaks)
    %#(define page-breaking ly:optimal-page-breaks)
    
    \include "engraver-init.ly"
}

#(set-default-paper-size "a4")


%{

; note:
; you can add fonts manually  in the paper block by issuing

#(set! fonts (append ...myfonts... fonts))

for the format of myfonts, see font.scm

%}

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

partCombineListener = \paper {
 \context {
	  \VoiceContext
	  \consists Note_heads_engraver
	  \consists Rest_engraver
	  \type "Recording_group_engraver"
	  recordEventSequence = #notice-the-events-for-pc
 }
 \context { \ScoreContext skipTypesetting = ##t }
}

#(set-part-combine-listener partCombineListener)

\include "dynamic-scripts-init.ly"
\include "spanners-init.ly"

\include "property-init.ly"



% reset default duration
unusedEntry = \notes { c4 }


% must have size argument for GUILE 1.6 compat.
#(define musicQuotes (make-hash-table 29)) 
