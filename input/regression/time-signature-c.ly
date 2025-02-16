\version "2.25.25"

\header {
  texidoc = "This test covers @code{TimeSignature@/.style} set to @code{C}.
Time signatures 4/4 and 2/2 are printed with the traditional common- and
cut-time symbols.  Other time signatures are printed as numbers.

The numbers above the staff show the input in each case."
}

#(ly:set-option 'warning-as-error #t)

#(define-markup-command (case-label layout props args)
  (markup-list?)
  (interpret-markup layout props
   (make-override-markup (cons 'baseline-skip 1.75)
    (make-center-column-markup args))))

\new Staff \with {
  \override TimeSignature.style = #'C
} \fixed c' {
  \tempo \markup \case-label { 4 4 }
  \time 4/4 d1

  \tempo \markup \case-label { 3 4 }
  \time 3/4 d2.

  \tempo \markup \case-label { 2 2 }
  \time 2/2 d1

  \tempo \markup \case-label { 2 ½ }
  \time #'(2 . 1/2) d\longa

  %% TODO: If it becomes possible to pass values like #'(2/3 . 2) to \time,
  %% this should be changed to do that instead of using \override.

  \tempo \markup \case-label { ⅔ 2 }
  \once \override Timing.TimeSignature.fraction = #'(2/3 . 2)
  \time 1/3 \tuplet 3/2 { e'4 d'4 }
}
