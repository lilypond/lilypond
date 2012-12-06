\version "2.17.6"

\header {
  lsrtags = "repeats, tweaks-and-overrides"

  texidoc = "
A @code{.|:} bar line can be printed at the beginning of a piece, by
overriding the relevant property:

"
  doctitle = "Printing a repeat sign at the beginning of a piece"
}


\relative c'' {
  \once \override Score.BreakAlignment.break-align-orders =
    #(make-vector 3 '(instrument-name
                      left-edge
                      ambitus
                      breathing-sign
                      clef
                      key-signature
                      time-signature
                      staff-bar
                      custos))
  \once \override Staff.TimeSignature.space-alist =
    #'((first-note . (fixed-space . 2.0))
       (right-edge . (extra-space . 0.5))
       ;; free up some space between time signature
       ;; and repeat bar line
       (staff-bar . (extra-space . 1)))
  \bar ".|:"
  c1
  d1
  d4 e f g
}
