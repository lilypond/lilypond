% 

longgrace = \override Stem  #'stroke-style = #'()
endlonggrace = \revert Stem #'stroke-style
ritenuto = \markup { \italic  "rit." }

\version "2.11.61"
  
cresc =  {
    #(ly:export (make-event-chord (list (make-span-event 'CrescendoEvent START)))) 
    \set crescendoText =  \markup { \italic \bold "cresc." }
    \set crescendoSpanner =  #'text
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
        \override VerticalAxisGroup #'minimum-Y-extent = #'(-2.5 . 3.5)
    }
}

\paper{

    % #(define fonts my-sheet)

    % stress page breaking on a6 paper:
    % line-width = 80 \mm
    % paper-width = 105 \mm
    % paper-height = 149 \mm
    
    indent = 10. \mm
    line-width = 189. \mm
    ragged-last-bottom = ##f

}

