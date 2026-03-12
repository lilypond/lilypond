\version "2.27.0"

\header {
  texidoc = "This test covers @code{TimeSignature@/.style} set to @code{C}.
Time signatures 4/4 and 2/2 are printed with the traditional common- and
cut-time symbols.  Other single-fraction time signatures are printed as
numbers.

Strictly alternating time signatures combining only 4/4 and 2/2 are also
printed with the traditional symbols separated by a thin space.  When other
fractions are present, only numbers are used.

The numbers above the staff show the input in each case."
}

#(ly:set-option 'warning-as-error #t)

#(define-markup-command (frac layout props args)
  (markup-list?)
  (interpret-markup layout props
   (make-override-markup (cons 'baseline-skip 1.75)
    (make-center-column-markup args))))

\paper {
  ragged-right = ##t
}

\new Staff \with {
  \override TimeSignature.style = #'C
  %% the EOL change warning is an unnecessary complication
  \override TimeSignature.break-visibility = #end-of-line-invisible
} \fixed c' {
  \tempo \markup \frac { 4 4 }
  \time 4/4 d1

  \tempo \markup \frac { 3 4 }
  \time 3/4 d2.

  \tempo \markup \frac { 2 2 }
  \time 2/2 d1

  \tempo \markup \frac { 2 1/2 }
  \time #'(2 . 1/2) d\longa

  \tempo \markup \frac { 2/3 2 }
  \time #'(2/3 . 2) \tuplet 3/2 { e'4 d'4 }

  \break

  %% example: Brahms. Fugue in A-flat minor, WoO 8.
  \tempo \markup { \frac { 4 4 } + \frac { 4 4 } }
  \time #'((4 . 4) (4 . 4)) d\breve

  %% example: Schubert. Impromptu Op. 90 No. 3, D. 899.
  \tempo \markup { \frac { 2 2 } + \frac { 2 2 } }
  \time #'((2 . 2) (2 . 2)) d\breve

  %% not seen in the wild; tests the generality of our implementation
  \tempo \markup { \frac { 2 2 } + \frac { 2 2 } + \frac { 2 2 } }
  \time #'((2 . 2) (2 . 2) (2 . 2)) d\breve.

  %% not seen in the wild; tests the generality of our implementation
  \tempo \markup { \frac { 2 2 } + \frac { 4 4 } }
  \time #'((2 . 2) (4 . 4)) d\breve

  %% not seen in the wild; tests the generality of our implementation
  \tempo \markup { \frac { 2 2 } + \frac { 3 2 } }
  \time #'((2 . 2) (3 . 2)) d1 d1.
}
