;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define-public (construct-chord-elements root duration modifications)
  " Build a chord on root using modifiers in MODIFICATIONS. NoteEvents
have duration DURATION.

Notes: natural 11 is left from chord if not explicitly specified.

Entry point for the parser.
"
  (let* ((flat-mods (flatten-list modifications))
	 (base-chord (stack-thirds (ly:make-pitch 0 4 0) the-canonical-chord))
	 (complete-chord '())
	 (bass #f)
	 (inversion #f)
	 (lead-mod #f)
	 (explicit-11 #f)
	 (start-additions #t))

    (define (interpret-inversion chord mods)
      "Read /FOO part. Side effect: INVERSION is set."
      (if (and (> (length mods) 1) (eq? (car mods) 'chord-slash))
	  (begin
	    (set! inversion (cadr mods))
	    (set! mods (cddr mods))))
      (interpret-bass chord mods))

    (define (interpret-bass chord mods)
      "Read /+FOO part. Side effect: BASS is set."
      (if (and (> (length mods) 1) (eq? (car mods) 'chord-bass))
	  (begin
	    (set! bass (cadr mods))
	    (set! mods (cddr mods))))
      (if (pair? mods)
	  (scm-error 'chord-format "construct-chord" "Spurious garbage following chord: ~A" mods #f))
      chord)

    (define (interpret-removals	 chord mods)
      (define (inner-interpret chord mods)
	(if (and (pair? mods) (ly:pitch? (car mods)))
	    (inner-interpret (remove-step (+ 1	(ly:pitch-steps (car mods))) chord)
			     (cdr mods))
	    (interpret-inversion chord mods)))
      (if (and (pair? mods) (eq? (car mods) 'chord-caret))
	  (inner-interpret chord (cdr mods))
	  (interpret-inversion chord mods)))

    (define (interpret-additions chord mods)
      "Interpret additions. TODO: should restrict modifier use?"
      (cond ((null? mods) chord)
	    ((ly:pitch? (car mods))
	     (if (= (pitch-step (car mods)) 11)
		 (set! explicit-11 #t))
	     (interpret-additions (cons (car mods) (remove-step (pitch-step (car mods)) chord))
				  (cdr mods)))
	    ((procedure? (car mods))
	     (interpret-additions ((car mods) chord)
				  (cdr mods)))
	    (else (interpret-removals chord mods))))

    (define (pitch-octavated-strictly-below p root)
      "return P, but octavated, so it is below	ROOT"
      (ly:make-pitch (+ (ly:pitch-octave root)
			(if (> (ly:pitch-notename root)
			       (ly:pitch-notename p))
			    0 -1))
		     (ly:pitch-notename p)
		     (ly:pitch-alteration p)))

    (define (process-inversion complete-chord)
      "Take out inversion from COMPLETE-CHORD, and put it at the bottom.
Return (INVERSION . REST-OF-CHORD).

Side effect: put original pitch in INVERSION.
If INVERSION is not in COMPLETE-CHORD, it will be set as a BASS, overriding
the bass specified.

"
      (let* ((root (car complete-chord))
	     (inv? (lambda (y)
		     (and (= (ly:pitch-notename y)
			     (ly:pitch-notename inversion))
			  (= (ly:pitch-alteration y)
			     (ly:pitch-alteration inversion)))))
	     (rest-of-chord (remove inv? complete-chord))
	     (inversion-candidates (filter inv? complete-chord))
	     (down-inversion (pitch-octavated-strictly-below inversion root)))
	(if (pair? inversion-candidates)
	    (set! inversion (car inversion-candidates))
	    (begin
	      (set! bass inversion)
	      (set! inversion #f)))
	(if inversion
	    (cons down-inversion rest-of-chord)
	    rest-of-chord)))
    ;; root is always one octave too low.
    ;; something weird happens when this is removed,
    ;; every other chord is octavated. --hwn... hmmm.
    (set! root (ly:pitch-transpose root (ly:make-pitch 1 0 0)))
    ;; skip the leading : , we need some of the stuff following it.
    (if (pair? flat-mods)
	(if (eq? (car flat-mods) 'chord-colon)
	    (set! flat-mods (cdr flat-mods))
	    (set! start-additions #f)))
    ;; remember modifier
    (if (and (pair? flat-mods) (procedure? (car flat-mods)))
	(begin
	  (set! lead-mod (car flat-mods))
	  (set! flat-mods (cdr flat-mods))))
    ;; extract first number if present, and build pitch list.
    (if (and (pair? flat-mods)
	     (ly:pitch?	 (car flat-mods))
	     (not (eq? lead-mod sus-modifier)))
	(begin
	  (if (= (pitch-step (car flat-mods)) 11)
	      (set! explicit-11 #t))
	  (set! base-chord
		(stack-thirds (car flat-mods) the-canonical-chord))
	  (set! flat-mods (cdr flat-mods))))
    ;; apply modifier
    (if (procedure? lead-mod)
	(set! base-chord (lead-mod base-chord)))
    (set! complete-chord
	  (if start-additions
	      (interpret-additions base-chord flat-mods)
	      (interpret-removals base-chord flat-mods)))
    (set! complete-chord (sort complete-chord ly:pitch<?))
    ;; If natural 11 + natural 3 is present, but not given explicitly,
    ;; we remove the 11.
    (if (and (not explicit-11)
	     (get-step 11 complete-chord)
	     (get-step 3 complete-chord)
	     (= 0 (ly:pitch-alteration (get-step 11 complete-chord)))
	     (= 0 (ly:pitch-alteration (get-step 3 complete-chord))))
	(set! complete-chord (remove-step 11 complete-chord)))
    ;; must do before processing inversion/bass, since they are
    ;; not relative to the root.
    (set! complete-chord (map (lambda (x) (ly:pitch-transpose x root))
			      complete-chord))
    (if inversion
	(set! complete-chord (process-inversion complete-chord)))
    (if bass
	(set! bass (pitch-octavated-strictly-below bass root)))
    (if #f
	(begin
	  (write-me "\n*******\n" flat-mods)
	  (write-me "root: " root)
	  (write-me "base chord: " base-chord)
	  (write-me "complete chord: " complete-chord)
	  (write-me "inversion: " inversion)
	  (write-me "bass: " bass)))
    (if inversion
	(make-chord-elements (cdr complete-chord) bass duration (car complete-chord)
		    inversion)
	(make-chord-elements complete-chord bass duration #f #f))))


(define (make-chord-elements pitches bass duration inversion original-inv-pitch)
  "Make EventChord with notes corresponding to PITCHES, BASS and
DURATION, and INVERSION."
  (define (make-note-ev pitch)
    (make-music 'NoteEvent
                'duration duration
                'pitch pitch))
  (let ((nots (map make-note-ev pitches))
	(bass-note (if bass (make-note-ev bass) #f))
	(inv-note (if inversion (make-note-ev inversion) #f)))
    (if bass-note
	(begin
	  (set! (ly:music-property bass-note 'bass) #t)
	  (set! nots (cons bass-note nots))))
    (if inv-note
	(begin
	  (set! (ly:music-property inv-note 'inversion) #t)
	  (set! (ly:music-property inv-note 'octavation)
		(- (ly:pitch-octave inversion)
		   (ly:pitch-octave original-inv-pitch)))
	  (set! nots (cons inv-note nots))))
    nots))

;;;;;;;;;;;;;;;;
; chord modifiers change the pitch list.

(define (aug-modifier pitches)
  (set! pitches (replace-step (ly:make-pitch 0 4 SHARP) pitches))
  (replace-step (ly:make-pitch 0 2 0) pitches))

(define (minor-modifier pitches)
  (replace-step (ly:make-pitch 0 2 FLAT) pitches))

(define (maj7-modifier pitches)
  (set! pitches (remove-step 7 pitches))
  (cons (ly:make-pitch 0 6 0) pitches))

(define (dim-modifier pitches)
  (set! pitches (replace-step (ly:make-pitch 0 2 FLAT) pitches))
  (set! pitches (replace-step (ly:make-pitch 0 4 FLAT) pitches))
  (set! pitches (replace-step (ly:make-pitch 0 6 DOUBLE-FLAT) pitches))
  pitches)

(define (sus-modifier pitches)
  (remove-step (pitch-step (ly:make-pitch 0 2 0)) pitches))

(define-safe-public default-chord-modifier-list
  `((m . ,minor-modifier)
    (min . ,minor-modifier)
    (aug . , aug-modifier)
    (dim . , dim-modifier)
    (maj . , maj7-modifier)
    (sus . , sus-modifier)))

;; canonical 13 chord.
(define the-canonical-chord
  (map (lambda (n)
	 (define (nca x)
	   (if (= x 7) FLAT 0))
	 
	 (if (>= n 8)
	     (ly:make-pitch 1 (- n 8) (nca n))
	     (ly:make-pitch 0 (- n 1) (nca n))))
       '(1 3 5 7 9 11 13)))

(define (stack-thirds upper-step base)
  "Stack thirds listed in BASE until we reach UPPER-STEP. Add
UPPER-STEP separately."
  (cond ((null? base) '())
	((> (ly:pitch-steps upper-step) (ly:pitch-steps (car base)))
	 (cons (car base) (stack-thirds upper-step (cdr base))))
	((<= (ly:pitch-steps upper-step) (ly:pitch-steps (car base)))
	 (list upper-step))
	(else '())))
