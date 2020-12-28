% -*- Scheme -*-

\header {
  texidoc = "This file profiles property accesses; the log file shows the top properties examined.
Note: lilypond must be compiled with --disable-optimising for this file to work."
}


#(ly:set-option 'profile-property-accesses #t)

\version "2.16.0"

\include "typography-demo.ly"
%\book { \score { {c4 } } }

#(define (prop-stats>?  x y)
  (cond
   ((> (cdr x) (cdr y)) #t)
   ((= (cdr x) (cdr y))
    (symbol<? (car x) (car y)))
   (else #f)))

#(define (display-stats what)
  (let*
   ((rnd 10)
    (round-to (lambda (x) (* rnd (inexact->exact (round (/ x rnd))))))
    (alist (map (lambda (entry) (cons (car entry) (round-to (cdr entry)))) (hash-table->alist (ly:property-lookup-stats what))))
    (total (apply + (map cdr alist)))
   )

  (set! alist (acons 'TOTAL total alist))
   
  (ly:progress "\n\n~A properties, rounded to ~a\n\n~a"
   what rnd
   (string-join
    (map (lambda (x) (format #f "~30a: ~6@a" (car x) (cdr x)))
     (sort alist prop-stats>?))
    "\n"))))
   

				 
#(display-stats 'prob)
#(display-stats 'context)
#(display-stats 'grob)

  
