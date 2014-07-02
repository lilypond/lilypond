\header {

  texidoc = "\\consists can take a scheme alist as arguments, which
  should be functions, which will be invoked as engraver functions."

}

\version "2.16.0"

#(define (t->m t)
   "Return the current moment of translator object @var{t}."
   (ly:context-current-moment (ly:translator-context t)))

engraver_demo =
#(make-engraver
   ((initialize translator)
    (format 1 "\n\n~16a: (initialize)\n" (t->m translator)))
   ((start-translation-timestep translator)
    (format 1 "~16a: (start-translation-timestep)\n" (t->m translator)))
   (listeners
     ((rest-event engraver event)
      (let ((grob (ly:engraver-make-grob engraver 'TextScript event)))
        (ly:grob-set-property! grob 'text "hi")
        (format 1 "~16a: detected this rest event: ~a\n~16a: created this grob: ~a\n"
                (t->m engraver) event (t->m engraver) grob))))
   (acknowledgers
     ((note-head-interface engraver grob source-engraver)
      (format 1 "~16a: saw ~a coming from ~a\n"
              (t->m engraver) grob source-engraver)))
   (end-acknowledgers
     ((beam-interface engraver grob source-engraver)
      (format 1 "~16a: saw end of ~a coming from ~a\n"
              (t->m engraver) grob source-engraver)))
   ((process-music translator)
    (format 1 "~16a: (process-music)\n" (t->m translator)))
   ((process-acknowledged translator)
    (format 1 "~16a: (process-acknowledged)\n" (t->m translator)))
   ((stop-translation-timestep translator)
    (format 1 "~16a: (stop-translation-timestep)\n" (t->m translator)))
   ((finalize translator)
    (format 1 "~16a: (finalize)\n" (t->m translator))))

\layout {
  \context {
    \Voice
    \consists
    \engraver_demo
  }
}

\relative c' {
  c8[ r c]
}
