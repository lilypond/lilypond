\header {

  texidoc = "\\consists can take a scheme alist as arguments, which
  should be functions, which will be invoked as engraver functions."

}

\version "2.23.9"

#(use-modules (ice-9 match))

#(define (t->m t)
   "Return the current moment of translator object @var{t}."
   (ly:context-current-moment (ly:translator-context t)))

% A slot can also be defined separately.
#(define (start-timestep-func translator)
   (format (current-error-port) "~16a: (start-translation-timestep)\n" (t->m translator)))

engraver_demo =
#(make-engraver
   ((initialize translator)
    (format (current-error-port) "\n\n~16a: (initialize)\n" (t->m translator)))
   (start-translation-timestep . start-timestep-func)
   (listeners
     ((rest-event engraver event)
      (let ((grob (ly:engraver-make-grob engraver 'TextScript event)))
        (ly:grob-set-property! grob 'text "hi")
        (format (current-error-port) "~16a: detected this rest event: ~a\n~16a: created this grob: ~a\n"
                (t->m engraver) event (t->m engraver) grob)))
     ((breathing-event engraver event #:once)
      (format (current-error-port) "~16a: detected breathing event in #:once listener: ~a"
              (t->m engraver) event)))
   (acknowledgers
     ((note-head-interface . args)
      (match-let (((engraver grob source-engraver) args))
        (format (current-error-port) "~16a: saw ~a coming from ~a\n"
                (t->m engraver) grob source-engraver))))
   (end-acknowledgers
     ((beam-interface engraver grob source-engraver)
      (format (current-error-port) "~16a: saw end of ~a coming from ~a\n"
              (t->m engraver) grob source-engraver)))
   ((pre-process-music translator)
    (format (current-error-port) "~16a: (pre-process-music)\n" (t->m translator)))
   ((process-music translator)
    (format (current-error-port) "~16a: (process-music)\n" (t->m translator)))
   ((process-acknowledged translator)
    (format (current-error-port) "~16a: (process-acknowledged)\n" (t->m translator)))
   ((stop-translation-timestep translator)
    (format (current-error-port) "~16a: (stop-translation-timestep)\n" (t->m translator)))
   ((finalize translator)
    (format (current-error-port) "~16a: (finalize)\n" (t->m translator))))

\layout {
  \context {
    \Voice
    \consists
    \engraver_demo
  }
}

#(ly:expect-warning (G_ "conflict with event: `~a'") 'breathing-event)
#(ly:expect-warning (G_ "discarding event: `~a'") 'breathing-event)

\relative {
  c'8[
  \breathe
  r
  % Equal events for #:once listener, the second just gets discarded.
  \breathe \breathe
  c]
  % Unequal events trigger a warning.
  \breathe
  \tweak color red \breathe
}
