\version "1.3.146"

%  german-chords.ly:
% german/norwegian/danish?

% To get Bb instead of B, use
% \include "german-chords.ly"
% #(set! german-Bb #t)

#(define german-Bb #f)

#(define (pitch->chord-name-text-banter pitch additions)
   (if (equal? (cdr pitch) '(6 -1))
     (if german-Bb
       (cons "B" (accidental->text -1))
       '("B")
     )
     (cons
       (list-ref '("C" "D" "E" "F" "G" "A" "H") (cadr pitch))
       (accidental->text (caddr pitch))
     )
   )
 )   


#(define (pitch->note-name-text-banter pitch)
   (if (equal? (cdr pitch) '(6 -1))
     '("b")
     (cons
       (string-append
	  (list-ref '("c" "d" "e" "f" "g" "a" "h") (cadr pitch))
	  (if (or (equal? (cadr pitch) 2) (equal? (cadr pitch) 5))
	    (list-ref '( "ses"  "s" "" "is" "isis") (+ 2 (caddr pitch)))
	    (list-ref '("eses" "es" "" "is" "isis") (+ 2 (caddr pitch)))
	  )
       )
       '()
     )
   )
 )
