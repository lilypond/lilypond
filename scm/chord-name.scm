;;;
;;; chord-name.scm --  chord name utility functions
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c)  2000--2003 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys

(define (natural-chord-alteration p)
  "Return the natural alteration for step P."
  (if (= (ly:pitch-steps p) 6)
      -1
      0))


(define-public (alteration->text-accidental-markup alteration)
  (make-smaller-markup
   (make-raise-markup
    (if (= alteration -1)
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
       (= alteration -1) 0.2
       )))


(define-public (note-name->markup pitch)
  "Return pitch markup for PITCH."
  (make-line-markup
   (list
    (make-simple-markup
     (vector-ref #("C" "D" "E" "F" "G" "A" "B") (ly:pitch-notename pitch)))
    (make-normal-size-super-markup
     (accidental->markup (ly:pitch-alteration pitch))))))


(define-public ((chord-name->german-markup B-instead-of-Bb) pitch)
  "Return pitch markup for PITCH, using german note names.
   If B-instead-of-Bb is set to #t real german names are returned.
   Otherwise semi-german names (with Bb and below keeping the british names)
"
  (let* ((name (ly:pitch-notename pitch))
         (alt (ly:pitch-alteration pitch))
	 (n-a (if (member (cons name alt) '((6 . -1) (6 . -2)))
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
	 (n-a (if (member (cons name alt) '((6 . -1) (6 . -2)))
		  (cons 7 (+ 1 alt))
		  (cons name alt))))
    (make-line-markup
     (list
      (string-append
       (list-ref '("c" "d" "e" "f" "g" "a" "h" "b") (car n-a))
       (if (or (equal? (car n-a) 2) (equal? (car n-a) 5))
	   (list-ref '( "ses"  "s" "" "is" "isis") (+ 2 (cdr n-a)))
	   (list-ref '("eses" "es" "" "is" "isis") (+ 2 (cdr n-a)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define-public (sequential-music-to-chord-exceptions seq omit-root)
  "Transform sequential music SEQ of type <<c d e>>-\markup{ foobar }
to (cons CDE-PITCHES FOOBAR-MARKUP), or to (cons DE-PITCHES
FOOBAR-MARKUP) if OMIT-ROOT.
"
  (define (chord-to-exception-entry m)
    (let* ((elts (ly:get-mus-property m 'elements))
	   (pitches (map (lambda (x) (ly:get-mus-property x 'pitch))
			 (filter-list
			  (lambda (y) (memq 'note-event
					    (ly:get-mus-property y 'types)))
			  elts)))
	   (sorted (sort pitches ly:pitch<?))
	   (root (car sorted))
	   (normalized (map (lambda (x) (ly:pitch-diff x root)) sorted))
	   (texts (map (lambda (x) (ly:get-mus-property x 'text))
		       (filter-list
			(lambda (y) (memq 'text-script-event
					  (ly:get-mus-property y 'types)))
			elts)))
	   (text (if (null? texts) #f (car texts))))
      (cons (if omit-root (cdr normalized) normalized) text)))

  (define (is-req-chord? m)
    (and
     (memq 'event-chord (ly:get-mus-property m 'types))
     (not (equal? (ly:make-moment 0 1) (ly:get-music-length m)))))

  (let* ((elts (filter-list is-req-chord? (ly:get-mus-property seq 'elements)))
	 (alist (map chord-to-exception-entry elts)))
    (filter-list (lambda (x) (cdr x)) alist)))


(define-public (new-chord-name-brew-molecule grob)
  (let*
      (
       (ws (ly:get-grob-property grob 'word-space))
       (markup (ly:get-grob-property grob 'text))
       (molecule (interpret-markup grob
				   (cons '((word-space . 0.0))
					 (Font_interface::get_property_alist_chain grob))
				   markup))
       )

    ;;
    ;; chord names aren't in staffs, so WS is in global staff space.
    (if (number? ws)
	(ly:molecule-combine-at-edge
	 molecule
	 X RIGHT (ly:make-molecule "" (cons 0 ws) '(-1 . 1) )
	 0.0)
	molecule)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (set-chord-name-style sym)
  "Return music expressions that set the chord naming style. For
inline use in .ly file"
  
  (define (chord-name-style-setter function exceptions)
    (context-spec-music
     (make-sequential-music 
      (list (make-property-set 'chordNameFunction function)
	    (make-property-set 'chordNameExceptions exceptions)))
     "ChordNames"
     )
    )

  (ly:export
   (case sym
     ((ignatzek)
      (chord-name-style-setter ignatzek-chord-names
			       '()))
     ((banter)
      (chord-name-style-setter double-plus-new-chord->markup-banter
       chord::exception-alist-banter))
     
     ((jazz)
      (chord-name-style-setter double-plus-new-chord->markup-jazz
       chord::exception-alist-jazz))
     )))

;; can't put this in double-plus-new-chord-name.scm, because we can't
;; ly:load that very easily.
(define-public (set-double-plus-new-chord-name-style style options)
  "Return music expressions that set the chord naming style. For
inline use in .ly file"
  
  (define (chord-name-style-setter function)
    (context-spec-music
     (make-sequential-music 
      (list (make-property-set 'chordNameFunction function)

	    ;; urg , misuse of chordNameExceptions function.
	    (make-property-set 'chordNameExceptions options)))
     "ChordNames"))

  (ly:export
   (case style
     ((banter)
      (chord-name-style-setter double-plus-new-chord->markup-banter))
     
     ((jazz)
      (chord-name-style-setter double-plus-new-chord->markup-jazz)))))

