\version "2.3.8"

% possible rename to scheme- or something like that.  -gp
\header { texidoc= "@cindex Scheme Manual Accidentals
The positions of accidentals may be manually set by incorporating
some Scheme code. " }

#(define (make-acc-position-checker pos)
  (lambda (elt)
   (and
      (not (eq? #f (memq 'accidental-interface
                    (ly:grob-property elt 'interfaces))))
      (eq? (ly:grob-property
	    (ly:grob-parent elt 1) 'staff-position) pos))))

\score {
  \context Voice  \relative c'' {
    c2.
    <<
\set Staff.AccidentalPlacement = \turnOff
\context Staff \applyoutput #(outputproperty-compatibility (make-acc-position-checker 9)
                               'extra-offset  '(-1 . 0))
\context Staff \applyoutput #(outputproperty-compatibility (make-acc-position-checker 5)
                               'extra-offset  '(-2 . 0))
\context Staff \applyoutput #(outputproperty-compatibility (make-acc-position-checker 3)
                               'extra-offset  '(-3 . 0))
\context Staff \applyoutput #(outputproperty-compatibility (make-acc-position-checker 2)
                               'extra-offset  '(-4 . 0))
      d!4
      eis
      gis
      d'!
    >>
  }
  \paper {
    raggedright = ##t
  }
}

