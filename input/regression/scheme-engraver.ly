\header {

  texidoc = "\\consists can take a scheme alist as arguments, which
  should be functions, which will be invoked as engraver functions."

}

\version "2.16.0"

\layout {
  \context {
    \Voice
    \consists
    #(make-engraver
      ((initialize trans)
       (display (list "initialize"
		      (ly:context-current-moment
		       (ly:translator-context trans)) "\n") (current-error-port)))
      ((start-translation-timestep trans)
       (display (list "start-trans"
		      (ly:context-current-moment
		       (ly:translator-context trans)) "\n") (current-error-port)))
      (listeners
       ((rest-event engraver event)
	(let*
	    ((x (ly:engraver-make-grob engraver 'TextScript event)))
	  (display (list "caught event" event "\ncreate:\n" x "\n") (current-error-port))
	  (ly:grob-set-property! x 'text "hi"))))
      (acknowledgers
       ((note-head-interface engraver grob source-engraver)
	(display (list "saw head: " grob " coming from " source-engraver) (current-error-port))))
      (end-acknowledgers
       ((beam-interface engraver grob source-engraver)
	(display (list "saw end of beam: " grob " coming from " source-engraver) (current-error-port))))
      ((process-music trans)
       (display (list "process-music"
		      (ly:context-current-moment
		       (ly:translator-context trans)) "\n") (current-error-port)))
      ((process-acknowledged trans)
       (display (list "process-acknowledged"
		      (ly:context-current-moment
		       (ly:translator-context trans)) "\n") (current-error-port)))
      ((stop-translation-timestep trans)
       (display (list "stop-trans"
		      (ly:context-current-moment
		       (ly:translator-context trans)) "\n") (current-error-port)))
      ((finalize trans)
       (display (list "finalize"
		      (ly:context-current-moment
		       (ly:translator-context trans)) "\n") (current-error-port))))
		}}


\relative c' {
  c8[ r c]
}
