\version "2.25.26"

\header {
  texidoc = "The @code{single-number} time-signature style prints the numerator
only.  The numbers above the staff show the input in each case."
}

#(ly:set-option 'warning-as-error #t)

#(define-markup-command (case-label layout props args)
  (markup-list?)
  (interpret-markup layout props
   (make-override-markup (cons 'baseline-skip 1.75)
    (make-center-column-markup args))))

\new Staff \with {
  \override TimeSignature.style = #'single-number
} \fixed c' {
  \tempo \markup \case-label { 1 2 }
  \time 1/2 d2

  \tempo \markup \case-label { 2 4 }
  \time 2/4 d4 d

  \tempo \markup \case-label { 3 4 }
  \time 3/4 d2.

  \tempo \markup \case-label { 16 4 }
  \time 16/4 d\longa

  \tempo \markup \case-label { 2 ½ }
  \time #'(2 . 1/2) d\longa

  \tempo \markup \case-label { ⅔ 2 }
  \time #'(2/3 . 2) \tuplet 3/2 { e'4 d'4 }
}
