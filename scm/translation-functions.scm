;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tuplets.

(define-public (denominator-tuplet-formatter mus)
  (number->string (ly:get-mus-property mus 'denominator)))

(define-public (fraction-tuplet-formatter mus)
  (string-append (number->string (ly:get-mus-property mus 'numerator))
		 ":"
		 (number->string (ly:get-mus-property mus 'denominator))
		 ))


;; metronome marks
(define-public (format-metronome-markup event context)
  (let*
      ((dur  (ly:get-mus-property event 'tempo-unit))
       (count (ly:get-mus-property event 'metronome-count))
       (note-mark (make-note-by-number-markup (ly:duration-log dur)
					      (ly:duration-dot-count dur)
					      1) ) ) 
    (make-line-markup
     (list
      note-mark
      (make-simple-markup  "=")
      (make-simple-markup (number->string count))
      
  ))))



(define number->mark-letter-vector (make-vector 25 #\A))

(do ((i 0 (1+ i))
     (j 0 (1+ j)) )
    ((>= i 26))
  (if (= i (- (char->integer #\I) (char->integer #\A)))
      (set! i (1+ i)))
  (vector-set! number->mark-letter-vector j
	       (integer->char (+ i (char->integer #\A))))  )

(define-public (number->mark-string n)
  "Double letters for big marks."
  (let*
      ((l (vector-length number->mark-letter-vector)))
    (display n) (newline)
    (display l) (newline)
    
  (if (>= n l)
      (string-append (number->mark-string (1- (quotient n l)))
		     (number->mark-string (remainder n l)))
      (make-string 1 (vector-ref number->mark-letter-vector n)))))


(define-public (format-mark-letters mark context)
  (make-bold-markup (number->mark-string (1- mark))))

(define-public (format-mark-numbers mark context)
  (make-bold-markup (number->string mark)))

