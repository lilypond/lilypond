

% #(set! point-and-click line-column-location)

longgrace = \property Voice.Stem \override #'flag-style = #'()
endlonggrace = \property Voice.Stem \revert #'flag-style
ritenuto = \textscript #'(italic  "rit.")

\version "1.5.47"

#(define italic-bf '((font-shape . italic) (font-series . bold)))

%% burp
%% the very idea of a style sheet, is that it's easy to override
#(define (set-style! sheet name style)
  (set-cdr! (assoc 'mark-letter (cdr (assoc 'style-alist sheet))) style))

#(define my-sheet (make-style-sheet 'paper20))
#(set-style! my-sheet 'mark-letter '((font-family . roman)
                                     (font-series . bold)
                                     (font-shape . upright)
                                     (font-relative-size . 3)))
  
cresc = \notes {
    \commandspanrequest \start "crescendo" 
    \property Voice.crescendoText = #`(,italic-bf "cresc.")
    \property Voice.crescendoSpanner = #'dashed-line
}

\paper{
    \stylesheet #my-sheet
    \translator {
        \ScoreContext
        skipBars = ##t
        midiInstrument = #"french horn"
        %% try to mimic Breitkopf
        RehearsalMark \override #'padding = #1
        MultiMeasureRest \override #'padding = #0.5
        MultiMeasureRest \override #'number-threshold = #1
        
        Beam \override #'thickness = #0.6
        Beam \override #'beam-space = #0.8
        Slur \override #'beautiful = #0.3
    }
    \translator {
        \StaffContext
        MinimumVerticalExtent = #'(-4.5 . 4.5)
    }
    indent = 10. \mm
    linewidth = 189. \mm
}
