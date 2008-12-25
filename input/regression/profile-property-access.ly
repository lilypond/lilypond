% -*- Scheme -*-

\header {
  texidoc = "This file profiles property accesses; the log file shows the top properties examined."
}


#(ly:set-option 'profile-property-accesses #t)

\version "2.12.0"

\include "../../input/typography-demo.ly"
%\book { \score { {c4 } } }

#(define (prop-stats>?  x y)
  (cond
   ((> (cdr x) (cdr y)) #t)
   ((= (cdr x) (cdr y))
    (symbol<? (car x) (car y)))
   (else #f)))

#(define (display-stats what)
  (let*
   ((count 50)
    (rnd 10)
    (round-to (lambda (x) (* rnd (inexact->exact (round (/ x rnd))))))
    (alist (map (lambda (entry) (cons (car entry) (round-to (cdr entry)))) (hash-table->alist (ly:property-lookup-stats what))))
    (total (apply + (map cdr alist)))
   )

  (set! alist (acons 'TOTAL total alist))
   
  (ly:progress "\n\n~A properties, top ~a rounded to ~a\n\n~a"
   what count rnd
   (string-join
    (map (lambda (x) (format "~30a: ~6@a" (car x) (cdr x)))
     (ly:truncate-list! 
    (sort alist prop-stats>?) count))
    "\n"))))
   

				 
#(display-stats 'prob)
#(display-stats 'context)
#(display-stats 'grob)

  
