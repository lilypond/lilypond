
%  german-chords.ly:
% german/norwegian/danish?


#(define (pitch->chord-name-text-banter pitch)
   (if (equal? (cdr pitch) '(6 0))
       '("H")
       (if (equal? (cdr pitch) '(6 -1)) '("B")
	   (pitch->text pitch))))

#(define (pitch->note-name-text-banter pitch)
   (if (equal? (cdr pitch) '(6 -1)) '("b")
       (cons
	(string-append
	 (if (equal? (cadr pitch) 6) "h"
	     (make-string 1 (integer->char (+ (modulo (+ (cadr pitch) 2) 7) 97))))
	 (case (caddr pitch)
	   ((-1) "eses")
	   ((-1) "es")
	   ((0) "")
	   ((1) "is")
	   ((2) "isis")))
	'())))
   

	     
\score {
  <
    \context ChordNames \chords { b1/+b bes/+bes bis/+bis }
    \notes\transpose c'' \chords { b1/+b bes/+bes bis/+bis }
  >
  \paper {
    linewidth = -1;
  }
}
	    
