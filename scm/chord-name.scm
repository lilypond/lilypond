;;;
;;; chord-name.scm --  chord name utility functions
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c)  2000--2003 Jan Nieuwenhuizen <janneke@gnu.org>
;;;
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define (natural-chord-alteration p)
  "Return the natural alteration for step P."
  (if (= (ly:pitch-steps p) 6)
      FLAT
      0))


;; 
;; TODO: make into markup.
;; 
(define-public (alteration->text-accidental-markup alteration)
  (make-smaller-markup
   (make-raise-markup
    (if (= alteration FLAT)
	0.3
	0.6)
    (make-musicglyph-markup
     (string-append "accidentals-" (number->string alteration))))))
  
(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (conditional-kern-before
       (alteration->text-accidental-markup alteration)
       (= alteration FLAT) 0.2
       )))


(define-public (note-name->markup pitch)
  "Return pitch markup for PITCH."
  (make-line-markup
   (list
    (make-simple-markup
     (vector-ref #("C" "D" "E" "F" "G" "A" "B") (ly:pitch-notename pitch)))
     (accidental->markup (ly:pitch-alteration pitch)))))


(define-public ((chord-name->german-markup B-instead-of-Bb) pitch)
  "Return pitch markup for PITCH, using german note names.
   If B-instead-of-Bb is set to #t real german names are returned.
   Otherwise semi-german names (with Bb and below keeping the british names)
"
  (let* ((name (ly:pitch-notename pitch))
         (alt (ly:pitch-alteration pitch))
	 (n-a (if (member (cons name alt) `((6 . ,FLAT) (6 . ,DOUBLE-FLAT)))
		 (cons 7 (+ (if B-instead-of-Bb 1 0) alt))
		 (cons name alt))))
    (make-line-markup
     (list
      (make-simple-markup
       (vector-ref #("C" "D" "E" "F" "G" "A" "H" "B") (car n-a)))
      (make-normal-size-super-markup
       (accidental->markup (cdr n-a)))))))


(define-public (note-name->german-markup  pitch)
  (let* ((name (ly:pitch-notename pitch))
	 (alt (ly:pitch-alteration pitch))
	 (n-a (if (member (cons name alt) `((6 . ,FLAT) (6 . ,DOUBLE-FLAT)))
		  (cons 7 (+ SEMI-TONE alt))
		  (cons name alt))))
    (make-line-markup
     (list
      (string-append
       (list-ref '("c" "d" "e" "f" "g" "a" "h" "b")  (car n-a) )
       (if (or (equal? (car n-a) 2) (equal? (car n-a) 5))
	   (list-ref '( "ses"  "s" "" "is" "isis") (+ 2 (/ (cdr n-a) 2) ))
	   (list-ref '("eses" "es" "" "is" "isis") (+ 2 (/ (cdr n-a) 2) ))
	   ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; fixme we should standardize on omit-root (or the other one.)
;; perhaps the  default should also be reversed --hwn
(define-public (sequential-music-to-chord-exceptions seq . rest)
  "Transform sequential music SEQ of type <<c d e>>-\\markup{ foobar }
to (cons CDE-PITCHES FOOBAR-MARKUP), or to (cons DE-PITCHES
FOOBAR-MARKUP) if OMIT-ROOT is given and non-false.
"

  (define (chord-to-exception-entry m)
    (let* ((elts (ly:get-mus-property m 'elements))
	   (omit-root (and (pair? rest) (car rest)))
	   (pitches (map (lambda (x) (ly:get-mus-property x 'pitch))
			 (filter
			  (lambda (y) (memq 'note-event
					    (ly:get-mus-property y 'types)))
			  elts)))
	   (sorted (sort pitches ly:pitch<?))
	   (root (car sorted))
	   
	   ;; ugh?
	   ;;(diff (ly:pitch-diff root (ly:make-pitch -1 0 0)))
	   ;; FIXME.  This results in #<Pitch c> ...,
	   ;; but that is what we need because default octave for
	   ;; \chords has changed to c' too?
	   (diff (ly:pitch-diff root (ly:make-pitch 0 0 0)))
	   (normalized (map (lambda (x) (ly:pitch-diff x diff)) sorted))
	   (texts (map (lambda (x) (ly:get-mus-property x 'text))
		       (filter
			(lambda (y) (memq 'text-script-event
					  (ly:get-mus-property y 'types)))
			elts)))

	   (text (if (null? texts) #f (if omit-root (car texts) texts))))
      (cons (if omit-root (cdr normalized) normalized) text)))

  (define (is-req-chord? m)
    (and
     (memq 'event-chord (ly:get-mus-property m 'types))
     (not (equal? (ly:make-moment 0 1) (ly:get-music-length m)))))

  (let* ((elts (filter is-req-chord? (ly:get-mus-property seq 'elements)))
	 (alist (map chord-to-exception-entry elts)))
    (filter (lambda (x) (cdr x)) alist)))

