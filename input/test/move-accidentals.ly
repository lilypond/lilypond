\header {

	 texidoc= "Positions of accidentals may be manually set. This
involves some scheme code."

}
#(define (make-acc-position-checker pos)
  (lambda (elt)
   (and
      (not (eq? #f (memq 'accidental-interface
                    (ly-get-grob-property elt 'interfaces))))
      (eq? (ly-get-grob-property
	    (ly-get-parent elt 1) 'staff-position) pos))))

\score {
  \context Voice \notes \relative c'' {
    c2.
    <
\property Staff.AccidentalPlacement = \turnOff
\context Staff \outputproperty #(make-acc-position-checker 9)
                               #'extra-offset = #'(-1 . 0)
\context Staff \outputproperty #(make-acc-position-checker 5)
                               #'extra-offset = #'(-2 . 0)
\context Staff \outputproperty #(make-acc-position-checker 3)
                               #'extra-offset = #'(-3 . 0)
\context Staff \outputproperty #(make-acc-position-checker 2)
                               #'extra-offset = #'(-4 . 0)
      d!4
      eis
      gis
      d'!
    >
  }
  \paper {
    linewidth = -1
  }
}
