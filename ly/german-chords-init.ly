\version "1.3.148"

%  german-chords.ly:
% german/norwegian/danish?

% To get Bb instead of B, use
% \include "german-chords.ly"
% #(set! german-Bb #t)

#(define german-Bb #f)

#(define (pitch->chord-name-text-banter pitch steps)
   (let ((dopitch (if (member (cdr pitch) '((6 -1) (6 -2)))
		      (list 7 (+ (if german-Bb 0 1) (caddr pitch)))
		      (cdr pitch)
		 )))
     (cons
       (list-ref '("C" "D" "E" "F" "G" "A" "H" "B") (car dopitch))
       (accidental->text-super (cadr dopitch))
     )
   )
 )



#(define (pitch->note-name-text-banter pitch)
   (let ((dopitch (if (member (cdr pitch) '((6 -1) (6 -2)))
		     (list 7 (+ 1 (caddr pitch)))
		     (cdr pitch)
		 )))
     (list
       (string-append
	  (list-ref '("c" "d" "e" "f" "g" "a" "h" "b") (car dopitch))
	  (if (or (equal? (car dopitch) 2) (equal? (car dopitch) 5))
	    (list-ref '( "ses"  "s" "" "is" "isis") (+ 2 (cadr dopitch)))
	    (list-ref '("eses" "es" "" "is" "isis") (+ 2 (cadr dopitch)))
	  )
       )
     )
   )
 )
