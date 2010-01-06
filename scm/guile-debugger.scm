(define-module ( scm guile-debugger)
  #:use-module  (ice-9 debugger)
  #:use-module  (ice-9 debugging traps)
  #:use-module  (ice-9 debugging trace)
  #:use-module  (ice-9 debugging steps)
  #:use-module  (ice-9 debugging ice-9-debugger-extensions)
  #:use-module  (ice-9 readline))
  #:export (	set-break!
				set-trace!
				set-trace-subtree)
(define (set-break! proc)
       (install-trap (make <procedure-trap>
                       #:procedure proc
                       #:behaviour debug-trap)))

(define (set-trace! proc)
       (install-trap (make <procedure-trap>
                       #:procedure proc
                       #:behaviour (list trace-trap
                                         trace-at-exit))))

(define (set-trace-subtree! proc)
       (install-trap (make <procedure-trap>
                       #:procedure proc
                       #:behaviour (list trace-trap
                                         trace-until-exit))))
