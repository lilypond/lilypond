

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

startGraceContextOrig = \startGraceContext

startGraceContext = {
    %% Huh?
    %% \startGraceContextOrig

    
    %%URG copy from original
    \property Voice.Stem \override  #'direction = #1
    \property Voice.Stem \override #'length = #6
    \property Voice.Stem \override #'lengths = 
        #(map (lambda (x) (* 0.8 x)) '(3.5 3.5 3.5 4.5 5.0))
    \property Voice.Stem \override #'beamed-lengths =
        #(map (lambda (x) (* 0.8 x)) '(0.0 2.5 2.0 1.5))
    \property Voice.Stem \override #'beamed-minimum-lengths =
        #(map (lambda (x) (* 0.8 x)) '(0.0 1.5 1.25 1.0))
    \property Voice.Stem \override #'no-stem-extend = ##t
    \property Voice.Stem \override #'flag-style  = #"grace"
    \property Voice.Beam \override #'thickness = #0.384

    %% Instead of calling Beam::space_function, we should invoke
    %% the previously active beam function...
    \property Voice.Beam \override #'space-function =
      #(lambda (beam mult) (* 0.8 (Beam::space_function beam mult)))

    \property Voice.Beam \override #'position-callbacks =
      #`(,Beam::least_squares
	 ,Beam::check_concave
	 ,Beam::slope_damping)
    
    % Can't use Staff.fontSize, since time sigs, keys sigs, etc. will
    % be smaller as well.

    \property Voice.fontSize = #-2
    \property Staff.Accidentals \override #'font-relative-size = #-2
    \property Voice.Slur \override #'direction = #-1
    %% end copy
    
    
    \property Voice.Beam \revert #'space-function
    \property Voice.Beam \override #'space-function
       = #(lambda (beam mult) (* 0.8 0.8))
    \property Voice.Beam \revert #'thickness
    \property Voice.Beam \override #'thickness = #(* 0.384 (/ 0.6 0.48))
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
        Beam \override #'space-function = #(lambda (beam mult) 0.8)
        Slur \override #'beautiful = #0.3
    }
    \translator {
        \StaffContext
        MinimumVerticalExtent = #'(-4.5 . 4.5)
    }
    indent = 10. \mm
    linewidth = 189. \mm

    % The piece should fit on 4 pages. 
    textheight = 275.\mm
}
