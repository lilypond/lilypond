\header {
texidoc = "context level weirdness.  after some commands, explicit
 \context Voice commands must be inserted; otherwise subsequent property
 settings fail silently"
}

% from les-nereides.ly

 #(define (make-text-checker text)
  (lambda (elt) (equal? text (ly-get-grob-property elt 'text))))

\score {
  \context PianoStaff <
    \context Staff=upper \notes\relative c' {
        \property Score.timing = ##f

        \outputproperty #(make-text-checker "foe") #'extra-offset = #'(-2 . 4)
        c-"foe"
	
	\translator Staff=lower

	% staff switch moves us
	% implicitely to staff level: text-checker won't see "foe"
        \outputproperty #(make-text-checker "foe") #'extra-offset = #'(-2 . 4)
        c-"foe"

	% get back to Voice level: it works again
	\context Voice
	\outputproperty #(make-text-checker "foe") #'extra-offset = #'(-2 . 4)
        c-"foe"

	\clef treble
	
	% clef change moves us
	% implicitely to staff level: text-checker won't see "foe"
        \outputproperty #(make-text-checker "foe") #'extra-offset = #'(-2 . 4)
        c-"foe"

	% get back to Voice level: it works again
	\context Voice
	\outputproperty #(make-text-checker "foe") #'extra-offset = #'(-2 . 4)
        c-"foe"
	
    }
    \context Staff=lower \notes\relative c' {
      s
    }
  >
  \paper {
    linewidth = -1.0
  }
}