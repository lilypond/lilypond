\header {

  texidoc = "Scheme engravers may be instantiated, with
  instance-scoped slots, by defining a 1 argument procedure which
  shall return the engraver definition as an alist, with the private
  slots defined in a closure.  The argument procedure argument is the
  context where the engraver is instantiated."

}

\version "2.19.21"

\layout {
  \context {
    \Voice
    \consists
    #(let ((instance-counter 0))
       (lambda (context)
         (set! instance-counter (1+ instance-counter))
         (let ((instance-id instance-counter)
               (private-note-counter 0))
	   (make-engraver
	    (listeners
              ((note-event engraver event)
	       (set! private-note-counter (1+ private-note-counter))
	       (let ((text (ly:engraver-make-grob engraver 'TextScript event)))
		 (ly:grob-set-property! text 'text
					(format #f "~a.~a" instance-id
						private-note-counter)))))))))
  }
}

<<
  \relative { c''4 d e f }
  \\ \relative { c'4 d e f }
>>
