\version "2.23.14"

\header {
  texidoc = "The 'stencil property of the Flag grob can be set to a custom
scheme function to generate the glyph for the flag."
}


% test notes, which will be shown in different style:
testnotes = { \autoBeamOff c'8 d'16 c'32 d'64 \acciaccatura {c'8} d'64 c''8 d''16 c''32 d''64 \acciaccatura {c''8} d''64  }

#(define-public (weight-flag grob)
  (let* ((stem-grob (ly:grob-parent grob X))
         (log (- (ly:grob-property stem-grob 'duration-log) 2))
         (is-up (eqv? (ly:grob-property stem-grob 'direction) UP))
         (yext (if is-up (cons (* log -0.8) 0) (cons 0 (* log 0.8))))
         (flag-stencil (make-filled-box-stencil '(-0.4 . 0.4) yext))
         (stroke-style (ly:grob-property grob 'stroke-style))
         (stroke-stencil (if (equal? stroke-style "grace") (make-line-stencil 0.2 -0.9 -0.4 0.9 -0.4) empty-stencil)))
    (ly:stencil-add flag-stencil stroke-stencil)))


% Create a flag stencil by looking up the glyph from the font
#(define (inverted-flag grob)
  (let* ((stem-grob (ly:grob-parent grob X))
         (dir (if (eqv? (ly:grob-property stem-grob 'direction) UP) "d" "u"))
         (flag (retrieve-glyph-flag "" dir "" grob))
         (line-thickness (ly:staff-symbol-line-thickness grob))
         (stem-thickness (ly:grob-property stem-grob 'thickness))
         (stem-width (* line-thickness stem-thickness))
         (stroke-style (ly:grob-property grob 'stroke-style))
         (stencil (if (null? stroke-style) flag
                         (add-stroke-glyph flag grob dir stroke-style "")))
         (rotated-flag (ly:stencil-rotate-absolute stencil 180 0 0)))
    (ly:stencil-translate rotated-flag (cons (- (/ stem-width 2))  0))))

{
  \time 2/4
  \textMark "Function: weight-flag (custom)"
  \override Flag.stencil = #weight-flag
  \testnotes

  \textMark "Function: inverted-flag (custom)"
  \override Flag.stencil = #inverted-flag
  \testnotes

}
