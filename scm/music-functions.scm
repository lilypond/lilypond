
(define (denominator-tuplet-formatter mus)
  (number->string (ly-get-mus-property mus 'denominator)))

(define (fraction-tuplet-formatter mus)
  (string-append (number->string (ly-get-mus-property mus 'numerator))
		 ":"
		 (number->string (ly-get-mus-property mus 'denominator))
		 ))

