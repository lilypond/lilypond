;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tuplets.

(define-public (denominator-tuplet-formatter mus)
  (number->string (ly:music-property mus 'denominator)))

(define-public (fraction-tuplet-formatter mus)
  (string-append
   (number->string (ly:music-property mus 'denominator))
   ":"
   (number->string (ly:music-property mus 'numerator))
   ))


;; metronome marks
(define-public (format-metronome-markup event context)
  (let*
      ((dur  (ly:music-property event 'tempo-unit))
       (count (ly:music-property event 'metronome-count))
       (note-mark (make-smaller-markup
		   (make-note-by-number-markup (ly:duration-log dur)
					      (ly:duration-dot-count dur)
					      1))))
    
    (make-line-markup
     (list
      (make-general-align-markup Y DOWN note-mark)
      (make-simple-markup  "=")
      (make-simple-markup (number->string count))
      
  ))))



(define-public (format-mark-letters mark context)
  (make-bold-markup (make-markletter-markup (1- mark))))

(define-public (format-mark-numbers mark context)
  (make-bold-markup (number->string mark)))

