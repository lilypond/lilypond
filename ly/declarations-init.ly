#(ly:set-option 'old-relative)

\version "2.1.13"
breve = #(ly:make-duration -1 0)
longa = #(ly:make-duration -2 0 )
maxima = #(ly:make-duration -3 0)

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

%{

should also set allowBeamBreak, but how to do it "portably"? (ie. also
working with lyric sections)

%}
break =#(make-event-chord (list (make-penalty-music -10001)))
noBreak =  #(make-event-chord (list (make-penalty-music 10001)))

\include "scale-definitions-init.ly"

melisma = \property Staff.melismaBusy = ##t
melismaEnd = \property Staff.melismaBusy = ##f


\include "grace-init.ly"

% ugh
\include "midi-init.ly"


% Do units first; must be done before any units are specified.
\paper {
    unit = "mm"
    mm = 1.0
    in = 25.4
    pt = #(/  in 72.27)
    cm = #(* 10 mm)

    texsetting = ""
    pssetting = ""
    scmsetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\n"% UGH. 


    #(define font-defaults
      '((font-family . music)
	(font-shape . upright)
	(baseline-skip . 2)
	(word-space . 0.6)
	(font-series . medium)
    ))
    
    \include "engraver-init.ly"
}

#(set-paper-size "a4")


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

\include "dynamic-scripts-init.ly"
\include "spanners-init.ly"

\include "property-init.ly"



% reset default duration
unusedEntry = \notes { c4 }

% music = "\melodic\relative c"

