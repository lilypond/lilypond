% #(ly:set-point-and-click 'line-column)

longgrace = \override Stem  #'stroke-style = #'()
endlonggrace = \revert Stem #'stroke-style
ritenuto = \markup { \italic  "rit." }

\version "2.3.22"
  
cresc =  {
    #(ly:export (make-event-chord (list (make-span-event 'CrescendoEvent START)))) 
    \set crescendoText =  \markup { \italic \bold "cresc." }
    \set crescendoSpanner =  #'dashed-line
}

%%
%% TODO: a better mechanism for tweaking Grace settings.
%%

startGraceMusic = \sequential { 
    \startGraceMusic 
    \override Beam   #'space-function
    = #(lambda (beam mult) (* 0.8 0.8))
    \override Beam   #'thickness = #(* 0.384 (/ 0.6 0.48))
}

stopGraceMusic= \sequential {
    \revert Beam #'thickness
    \revert Beam #'space-function
    \stopGraceMusic
}

\layout {
    \context {
        \Score
        skipBars = ##t
        midiInstrument = #"french horn"
        %% try to mimic Breitkopf
        \override RehearsalMark #'padding = #1
        \override MultiMeasureRest #'padding = #0.5
        restNumberThreshold = #1

	\override RehearsalMark #'font-series = #'bold
	\override RehearsalMark #'font-size = #4.5

        \override Beam #'thickness = #0.6
        \override Beam #'space-function = #(lambda (beam mult) 0.8)
    }
    \context {
        \Staff
        minimumVerticalExtent = #'(-2.5 . 3.5)
    }
}

\paper{

    % #(define fonts my-sheet)

    % stress page breaking on a6 paper:
    % linewidth = 80 \mm
    % hsize = 105 \mm
    % vsize = 149 \mm
    
    indent = 10. \mm
    linewidth = 189. \mm

}

