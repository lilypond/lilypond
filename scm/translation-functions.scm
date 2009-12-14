;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;		     Jan Nieuwenhuizen <janneke@gnu.org>
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metronome marks

(define-public (format-metronome-markup text dur count context)
  (let* ((hide-note (eq? #t (ly:context-property context 'tempoHideNote))))
    (metronome-markup text dur count hide-note)))

(define-public (metronome-markup text dur count hide-note)
  (let* ((note-mark (if (and (not hide-note) (ly:duration? dur))
			(make-smaller-markup
			 (make-note-by-number-markup (ly:duration-log dur)
						     (ly:duration-dot-count dur)
						     1))
			#f))
         (note-markup (if (and (not hide-note) (number? count) (> count 0) )
			  (make-concat-markup
			   (list
			    (make-general-align-markup Y DOWN note-mark)
			    (make-simple-markup " ")
			    (make-simple-markup "=")
			    (make-simple-markup " ")
			    (make-simple-markup (number->string count))))
			  #f))
         (text-markup (if (not (null? text))
			  (make-bold-markup text)
			  #f)))
    (if text-markup
	(if (and note-markup (not hide-note))
	    (make-line-markup (list text-markup
				    (make-concat-markup
				     (list (make-simple-markup "(")
					   note-markup
					   (make-simple-markup ")")))))
	    (make-line-markup (list text-markup)))
	(if note-markup
	    (make-line-markup (list note-markup))
	    (make-null-markup)))))

(define-public (format-mark-alphabet mark context)
  (make-bold-markup (make-markalphabet-markup (1- mark))))

(define-public (format-mark-box-alphabet mark context)
  (make-bold-markup (make-box-markup (make-markalphabet-markup (1- mark)))))

(define-public (format-mark-circle-alphabet mark context)
  (make-bold-markup (make-circle-markup (make-markalphabet-markup (1- mark)))))

(define-public (format-mark-letters mark context)
  (make-bold-markup (make-markletter-markup (1- mark))))

(define-public (format-mark-numbers mark context)
  (make-bold-markup (number->string mark)))

(define-public (format-mark-barnumbers mark context)
  (make-bold-markup (number->string (ly:context-property context
							 'currentBarNumber))))

(define-public (format-mark-box-letters mark context)
  (make-bold-markup (make-box-markup (make-markletter-markup (1- mark)))))

(define-public (format-mark-circle-letters mark context)
  (make-bold-markup (make-circle-markup (make-markletter-markup (1- mark)))))

(define-public (format-mark-box-numbers mark context)
  (make-bold-markup (make-box-markup (number->string mark))))

(define-public (format-mark-circle-numbers mark context)
  (make-bold-markup (make-circle-markup (number->string mark))))

(define-public (format-mark-box-barnumbers mark context)
  (make-bold-markup (make-box-markup
		     (number->string (ly:context-property context
							  'currentBarNumber)))))

(define-public (format-mark-circle-barnumbers mark context)
  (make-bold-markup (make-circle-markup
		     (number->string (ly:context-property context
							  'currentBarNumber)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bass figures.

(define-public (format-bass-figure figure event context)
  (let* ((fig (ly:event-property event 'figure))
	 (fig-markup (if (number? figure)

			 ;; this is not very elegant, but center-aligning
			 ;; all digits is problematic with other markups,
			 ;; and shows problems in the (lack of) overshoot
			 ;; of feta-alphabet glyphs.
			 ((if (<= 10 figure)
			      (lambda (y) (make-translate-scaled-markup
					   (cons -0.7 0) y))
			      identity)

			  (cond
			   ((eq? #t (ly:event-property event 'diminished))
			    (markup #:slashed-digit figure))
			   ((eq? #t (ly:event-property event 'augmented-slash))
			    (markup #:backslashed-digit figure))
			   (else (markup #:number (number->string figure 10)))))
			 #f))

	 (alt (ly:event-property event 'alteration))
	 (alt-markup
	  (if (number? alt)
	      (markup
	       #:general-align Y DOWN #:fontsize
	       (if (not (= alt DOUBLE-SHARP))
		   -2 2)
	       (alteration->text-accidental-markup alt))
	      #f))

	 (plus-markup (if (eq? #t (ly:event-property event 'augmented))
			  (markup #:number "+")
			  #f))

	 (alt-dir (ly:context-property context 'figuredBassAlterationDirection))
	 (plus-dir (ly:context-property context 'figuredBassPlusDirection)))

    (if (and (not fig-markup) alt-markup)
	(begin
	  (set! fig-markup (markup #:left-align #:pad-around 0.3 alt-markup))
	  (set! alt-markup #f)))


    ;; hmm, how to get figures centered between note, and
    ;; lone accidentals too?

    ;;    (if (markup? fig-markup)
    ;;	(set!
    ;;	 fig-markup (markup #:translate (cons 1.0 0)
    ;;			    #:center-align fig-markup)))

    (if alt-markup
	(set! fig-markup
	      (markup #:put-adjacent
		      X (if (number? alt-dir)
			    alt-dir
			    LEFT)
		      fig-markup
		      #:pad-x 0.2 alt-markup)))

    (if plus-markup
	(set! fig-markup
	      (if fig-markup
		  (markup #:put-adjacent
			  X (if (number? plus-dir)
				plus-dir
				LEFT)
			  fig-markup
			  #:pad-x 0.2 plus-markup)
		  plus-markup)))

    (if (markup? fig-markup)
	(markup #:fontsize -2 fig-markup)
	empty-markup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fret diagrams

(define-public (determine-frets context grob notes string-numbers)

  (define (ensure-number a b)
    (if (number? a)
	a
	b))

  (define (string-frets->dot-placement string-frets string-count)
    (let* ((desc (list->vector
		  (map (lambda (x) (list 'mute  (1+ x)))
		       (iota string-count)))))

      (for-each (lambda (sf)
		  (let* ((string (car sf))
			 (fret (cadr sf))
			 (finger (caddr sf)))

		    (vector-set!
		     desc (1- string)
		     (if (= 0 fret)
			 (list 'open string)
			 (if finger
			     (list 'place-fret string fret finger)
			     (list 'place-fret string fret))))))
		string-frets)

      (vector->list desc)))

  (define (get-predefined-fretboard predefined-fret-table tuning pitches)
;   (_i "Search through @var{predefined-fret-table} looking for a predefined
;fretboard with a key of @var{(tuning . pitches)}.  The search will check
;both up and down an octave in order to accomodate transposition of the
;chords.")
    (define (get-fretboard key)
      (let ((hash-handle
	     (hash-get-handle predefined-fret-table key)))
	(if hash-handle
	    (cdr hash-handle)  ; return table entry
	    '())))

    (let ((test-fretboard (get-fretboard (cons tuning pitches))))
      (if (not (null? test-fretboard))
	  test-fretboard
	  (let ((test-fretboard
		 (get-fretboard
		  (cons tuning (map (lambda (x) (shift-octave x 1)) pitches)))))
	    (if (not (null? test-fretboard))
		test-fretboard
		(get-fretboard
		 (cons tuning (map (lambda (x) (shift-octave x -1))
				   pitches))))))))

;; body.
  (let* ((tunings (ly:context-property context 'stringTunings))
	 (my-string-count (length tunings))
	 (details (ly:grob-property grob 'fret-diagram-details))
	 (predefined-frets
	  (ly:context-property context 'predefinedDiagramTable))
	 (pitches (map (lambda (x) (ly:event-property x 'pitch)) notes))
         (predefined-fretboard
          (if predefined-frets
              (get-predefined-fretboard
               predefined-frets
               tunings
               pitches)
              '())))

    (set! (ly:grob-property grob 'fret-diagram-details)
          (if (null? details)
              (acons 'string-count my-string-count '())
              (acons 'string-count my-string-count details)))

    (set! (ly:grob-property grob 'dot-placement-list)
          (if (not (null? predefined-fretboard))
              predefined-fretboard
              (let* ((minimum-fret
                      (ensure-number
                       (ly:context-property context 'minimumFret)
                       0))
                     (max-stretch
                      (ensure-number
                       (ly:context-property context 'maximumFretStretch)
                       4))
                     (string-frets
                      (determine-frets-mf
                       notes
                       string-numbers
                       minimum-fret
                       max-stretch
                       tunings)))
		(string-frets->dot-placement
		  string-frets
                  my-string-count))))))

(define-public (determine-frets-mf notes string-numbers
				   minimum-fret max-stretch
				   tunings)

  (define (calc-fret pitch string tuning)
    (- (ly:pitch-semitones pitch) (list-ref tuning (1- string))))

  (define (note-pitch a)
    (ly:event-property a 'pitch))

  (define (note-pitch>? a b)
    (ly:pitch<? (note-pitch b)
		(note-pitch a)))

  (define (note-finger ev)
    (let* ((articulations (ly:event-property ev 'articulations))
	   (finger-found #f))

      (map (lambda (art)
	     (let* ((num (ly:event-property art 'digit)))

	       (if (and (eq? 'fingering-event (ly:event-property art 'class))
			(number? num))
		   (set! finger-found num))))
	   articulations)

      finger-found))

  (define (note-string ev)
    (let* ((articulations (ly:event-property ev 'articulations))
	   (string-found #f))

      (map (lambda (art)
	     (let* ((num (ly:event-property art 'string-number)))

	       (if (number? num)
		   (set! string-found num))))
	   articulations)

      string-found))

  (define (del-string string)
    (if (number? string)
	(set! free-strings
	      (delete string free-strings))))

  (define specified-frets '())
  (define free-strings '())

  (define (close-enough fret)
    (reduce
     (lambda (x y)
       (and x y))
     #t
     (map (lambda (specced-fret)
	    (> max-stretch (abs (- fret specced-fret))))
	  specified-frets)))

  (define (string-qualifies string pitch)
    (let* ((fret (calc-fret pitch string tunings)))
      (and (>= fret minimum-fret)
	   (close-enough fret))))

  (define string-fret-fingering-tuples '())
  (define (set-fret note string)
    (set! string-fret-fingering-tuples
	  (cons (list string
		      (calc-fret (ly:event-property note 'pitch)
				 string tunings)
		      (note-finger note))
		string-fret-fingering-tuples))
    (del-string string))


  ;;; body.
  (set! specified-frets
	(filter identity (map
			  (lambda (note)
			    (if (note-string note)
				(calc-fret (note-pitch note)
					   (note-string note) tunings)
				#f))
			  notes)))

  (set! free-strings (map 1+ (iota (length tunings))))

  (for-each (lambda (note)
	      (del-string (note-string note)))
	    notes)


  (for-each
   (lambda (note)
     (if (note-string note)
	 (set-fret note (note-string note))
	 (let* ((fit-string (find (lambda (string)
				    (string-qualifies string (note-pitch note)))
				  free-strings)))
	   (if fit-string
	       (set-fret note fit-string)
	       (ly:warning "No string for pitch ~a (given frets ~a)"
                           (note-pitch note)
			   specified-frets)))))
   (sort notes note-pitch>?))

  string-fret-fingering-tuples)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tablature

;; The TabNoteHead tablatureFormat callbacks.

;; Calculate the fret from pitch and string number as letter
;; The fret letter is taken from 'fretLabels if present
(define-public (fret-letter-tablature-format string-number context event)
  (let* ((tuning (ly:context-property context 'stringTunings))
         (pitch (ly:event-property event 'pitch))
         (labels (ly:context-property context 'fretLabels))
         (fret (- (ly:pitch-semitones pitch)
                  (list-ref tuning (- string-number 1)))))
    (make-vcenter-markup
     (cond
      ((= 0 (length labels))
       (string (integer->char (+ fret (char->integer #\a)))))
      ((and (<= 0 fret) (< fret (length labels)))
       (list-ref labels fret))
      (else
       (ly:warning "No label for fret ~a (~a on string ~a);
only ~a fret labels provided"
		   fret pitch string-number (length labels))
       ".")))))

;; Calculate the fret from pitch and string number as number
(define-public (fret-number-tablature-format string-number context event)
  (let* ((tuning (ly:context-property context 'stringTunings))
	 (pitch (ly:event-property event 'pitch)))
    (make-vcenter-markup
     (format
      "~a"
      (- (ly:pitch-semitones pitch)
         (list-ref tuning
                   ;; remove 1 because list index starts at 0
                   ;;and guitar string at 1.
                   (1- string-number)))))))

;; The 5-string banjo has got a extra string, the fifth (duh), which
;; starts at the fifth fret on the neck.  Frets on the fifth string
;; are referred to relative to the other frets:
;;   the "first fret" on the fifth string is really the sixth fret
;;   on the banjo neck.
;; We solve this by defining a new fret-number-tablature function:
(define-public (fret-number-tablature-format-banjo string-number context event)
  (let* ((tuning (ly:context-property context 'stringTunings))
	 (pitch (ly:event-property event 'pitch)))
    (make-vcenter-markup
      (let ((fret (- (ly:pitch-semitones pitch)
                     (list-ref tuning (1- string-number)))))
        (number->string (cond
                          ((and (> fret 0) (= string-number 5))
                            (+ fret 5))
                          (else fret)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar numbers

(define-public ((every-nth-bar-number-visible n) barnum)
  (= 0 (modulo barnum n)))

(define-public ((modulo-bar-number-visible n m) barnum)
  (and (> barnum 1) (= m (modulo barnum n))))

(define-public ((set-bar-number-visibility n) tr)
  (let ((bn (ly:context-property tr 'currentBarNumber)))
    (ly:context-set-property! tr 'barNumberVisibility
			      (modulo-bar-number-visible n (modulo bn n)))))

(define-public (first-bar-number-invisible barnum) (> barnum 1))

(define-public (all-bar-numbers-visible barnum) #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; percent repeat counters

(define-public ((every-nth-repeat-count-visible n) count context)
  (= 0 (modulo count n)))

(define-public (all-repeat-counts-visible count context) #t)
