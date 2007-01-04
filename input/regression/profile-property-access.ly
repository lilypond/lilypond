\header {
  texidoc = "This file profiles property accesses; the log file shows the top properties examined."
}


#(ly:set-option 'profile-property-accesses #t)

\version "2.10.8"

\include "../../input/typography-demo.ly"
%\book { \score { {c4 } } }

#(define (prop-stats>?  x y) (> (cdr x) (cdr y)))

#(define (display-stats what hash)
  (let*
   ((count 50)
    (rnd 10))
  (ly:progress "\n~A properties, top ~a rounded to ~a\n~a"
   what count rnd
   (string-join
    (map (lambda (x) (format "~30a: ~6@a" (car x) (* rnd (inexact->exact (round (/ (cdr x) rnd))))))
     (take 
    (sort (hash-table->alist hash) prop-stats>?) count))
    "\n"))))
   

				 
#(display-stats "Context" (ly:context-property-lookup-stats))

#(display-stats "Grob" (ly:grob-property-lookup-stats))

  
