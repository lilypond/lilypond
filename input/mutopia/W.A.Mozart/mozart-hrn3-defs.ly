

% #(ly:set-point-and-click 'line-column)

longgrace = \property Voice.Stem \override #'stroke-style = #'()
endlonggrace = \property Voice.Stem \revert #'stroke-style
ritenuto = \textscript #'(italic  "rit.")

\version "1.7.3"

#(define italic-bf '((font-shape . italic) (font-series . bold)))

%% burp
%% the very idea of a style sheet, is that it's easy to override
%#(define (set-style! sheet name style)
%  (set-cdr! (assoc 'mark-letter (cdr (assoc 'style-alist sheet))) style))
%
%#(define my-sheet (make-font-list 'paper20))
%#(set-style! my-sheet 'mark-letter '((font-family . roman)
%                                     (font-series . bold)
%                                     (font-shape . upright)
%                                     (font-relative-size . 3)))

#(define (set-style! xstyle-alist name style)
  (set-cdr! (assoc name xstyle-alist)) style)

#(set-style! style-alist 'mark-letter '((font-family . roman)
                                        (font-series . bold)
                                        (font-shape . upright)
                                        (font-relative-size . 3)))
  
  
  
cresc = \notes {
    #(ly:export (make-event-chord (list (make-span-event 'CrescendoEvent START)))) 
    \property Voice.crescendoText = #`(,italic-bf "cresc.")
    \property Voice.crescendoSpanner = #'dashed-line
}

%%
%% TODO: a better mechanism for tweaking Grace settings.
%%

startGraceMusic = \sequential { 
    \startGraceMusic 
    \property Voice.Beam \override #'space-function
       = #(lambda (beam mult) (* 0.8 0.8))
    \property Voice.Beam \override #'thickness = #(* 0.384 (/ 0.6 0.48))
}

stopGraceMusic= \sequential {
    \property Voice.Beam \revert #'thickness
    \property Voice.Beam \revert #'space-function
    \stopGraceMusic
}

\paper{
    #(define fonts my-sheet)
    \translator {
        \ScoreContext
        skipBars = ##t
        midiInstrument = #"french horn"
        %% try to mimic Breitkopf
        RehearsalMark \override #'padding = #1
        MultiMeasureRest \override #'padding = #0.5
        MultiMeasureRest \override #'number-threshold = #1
        
        Beam \override #'thickness = #0.6
        Beam \override #'space-function = #(lambda (beam mult) 0.8)
        Slur \override #'beautiful = #0.3
    }
    \translator {
        \StaffContext
        minimumVerticalExtent = #'(-4.5 . 4.5)
    }
    indent = 10. \mm
    linewidth = 189. \mm

}
