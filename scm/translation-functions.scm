;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; (c) 1998--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
	 (count-markup (cond ((number? count)
			      (if (> count 0)
				  (make-simple-markup (number->string count))
				  #f))
			     ((pair? count)
			      (make-concat-markup
			       (list
				(make-simple-markup (number->string (car count)))
				(make-simple-markup " ")
				(make-simple-markup "–")
				(make-simple-markup " ")
				(make-simple-markup (number->string (cdr count))))))
			     (else #f)))
         (note-markup (if (and (not hide-note) count-markup)
			  (make-concat-markup
			   (list
			    (make-general-align-markup Y DOWN note-mark)
			    (make-simple-markup " ")
			    (make-simple-markup "=")
			    (make-simple-markup " ")
			    count-markup))
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

(define (create-fretboard context grob placement-list)
  "Convert @var{placement-list} into a fretboard @var{grob}."

  (let* ((tunings (ly:context-property context 'stringTunings))
	 (my-string-count (length tunings))
	 (details (ly:grob-property grob 'fret-diagram-details)))

    ;; Add string-count from string-tunings to fret-diagram-details.
    (set! (ly:grob-property grob 'fret-diagram-details)
            (acons 'string-count my-string-count details))
    ;; Create the dot-placement list for the grob
    (set! (ly:grob-property grob 'dot-placement-list) placement-list)))

(define-public
  (determine-frets context notes specified-info . rest)
  "Determine string numbers and frets for playing @var{notes}
as a chord, given specified information  @var{specified-info}.
@var{specified-info} is a list with two list elements,
specified strings @var{defined-strings} and
specified fingerings @var{defined-fingers}.  Only a fingering of
0 will affect the fret selection, as it specifies an open string.
If @var{defined-strings} is @code{'()}, the context property
@code{defaultStrings} will be used as a list of defined strings.
Will look for predefined fretboards if @code{predefinedFretboardTable}
is not @code {#f}.  If @var{rest} is present, it contains the
FretBoard grob, and a fretboard will be
created.  Otherwise, a list of (string fret finger) lists will
be returned)."

  ;;  helper functions

  (define (string-frets->placement-list string-frets string-count)
    "Convert @var{string-frets} to @code{fret-diagram-verbose}
dot placement entries."
    (let* ((placements (list->vector
                        (map (lambda (x) (list 'mute  (1+ x)))
		             (iota string-count)))))

      (for-each (lambda (sf)
		  (let* ((string (car sf))
			 (fret (cadr sf))
			 (finger (caddr sf)))
		    (vector-set!
                     placements (1- string)
		     (if (= 0 fret)
                         (list 'open string)
			 (if finger
			     (list 'place-fret string fret finger)
			     (list 'place-fret string fret))))))
		string-frets)
      (vector->list placements)))

  (define (placement-list->string-frets placement-list)
    "Convert @var{placement-list} to string-fret list."
    (map (lambda (x) (if (eq? (car x) 'place-fret)
                         (cdr x)
                         (list (cadr x) 0)))
         (filter (lambda (l) (or (eq? (car l) 'place-fret)
                                 (eq? (car l) 'open)))
                 placement-list)))

  (define (entry-count art-list)
    (length (filter (lambda (x) (not (null? x)))
                    art-list)))

  (define (get-predefined-fretboard predefined-fret-table tuning pitches)
    "Search through @var{predefined-fret-table} looking for a predefined
fretboard with a key of @var{(tuning . pitches)}.  The search will check
both up and down an octave in order to accomodate transposition of the
chords.  Returns a placement-list."

    (define (get-fretboard key)
      (let ((hash-handle
	     (hash-get-handle predefined-fret-table key)))
	(if hash-handle
	    (cdr hash-handle)  ; return table entry
	    '())))

    ;; body of get-predefined-fretboard
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

  ;; body of determine-frets
  (let* ((predefined-fret-table
	  (ly:context-property context 'predefinedDiagramTable))
         (tunings (ly:context-property context 'stringTunings))
         (string-count (length tunings))
         (grob (if (null? rest) '() (car rest)))
	 (pitches (map (lambda (x) (ly:event-property x 'pitch)) notes))
         (defined-strings (map (lambda (x)
                                 (if (null? x)
                                     x
                                     (ly:event-property x 'string-number)))
                               (car specified-info)))
         (defined-fingers (map (lambda (x)
                                 (if (null? x)
                                     x
                                     (ly:event-property x 'digit)))
                               (cadr specified-info)))
         (default-strings (ly:context-property context 'defaultStrings '()))
         (strings-used (if (and (zero? (entry-count defined-strings))
                                (not (zero? (entry-count default-strings))))
                           default-strings
                           defined-strings))
         (predefined-fretboard
          (if predefined-fret-table
              (get-predefined-fretboard
               predefined-fret-table
               tunings
               pitches)
              '())))
     (if (null? predefined-fretboard)
         (let ((string-frets
                (determine-frets-and-strings
                 notes
                 strings-used
                 defined-fingers
                 (ly:context-property context 'minimumFret 0)
                 (ly:context-property context 'maximumFretStretch 4)
                 tunings)))
            (if (null? grob)
                string-frets
                (create-fretboard
                 context grob (string-frets->placement-list
                                string-frets string-count))))
         (if (null? grob)
             (placement-list->string-frets predefined-fretboard)
             (create-fretboard context grob predefined-fretboard)))))


(define (determine-frets-and-strings
          notes
          defined-strings
          defined-fingers
          minimum-fret
          maximum-stretch
          tuning)

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
			(number? num)
                        (> num 0))
		   (set! finger-found num))))
	   articulations)

      finger-found))

  (define (string-number event)
    (let ((num (ly:event-property event 'string-number)))
      (if (number? num)
          num
          #f)))

  (define (delete-free-string string)
    (if (number? string)
	(set! free-strings
	      (delete string free-strings))))

  (define free-strings '())
  (define unassigned-notes '())
  (define specified-frets '())

  (define (close-enough fret)
    (if (null? specified-frets)
        #t
        (reduce
          (lambda (x y)
            (and x y))
          #t
          (map (lambda (specced-fret)
                 (or (eq? 0 specced-fret)
                     (>= maximum-stretch (abs (- fret specced-fret)))))
               specified-frets))))

  (define (string-qualifies string pitch)
    (let* ((fret (calc-fret pitch string tuning)))
      (and (>= fret minimum-fret)
	   (close-enough fret))))

  (define (open-string string pitch)
    (let* ((fret (calc-fret pitch string tuning)))
      (eq? fret 0)))

  (define string-fret-fingering-tuples '())

  (define (set-fret note string)
    (let ((this-fret (calc-fret (ly:event-property note 'pitch)
                                string
                                tuning)))
       (if (< this-fret 0)
           (ly:warning (_ "Negative fret for pitch ~a on string ~a")
                                       (note-pitch note) string))
       (set! string-fret-fingering-tuples
             (cons (list string
                         this-fret
                         (note-finger note))
                   string-fret-fingering-tuples))
       (delete-free-string string)
       (set! specified-frets (cons this-fret specified-frets))))

  (define (pad-list target template)
    (while (< (length target) (length template))
           (set! target (if (null? target)
                            '(())
                            (append target '(()))))))

  ;;; body of determine-frets-and-strings
  (set! free-strings (map 1+ (iota (length tuning))))

  ;; get defined-strings same length as notes
  (pad-list defined-strings notes)

  ;; get defined-fingers same length as notes
  (pad-list defined-fingers notes)

  ;; handle notes with strings assigned and fingering of 0
  (for-each
    (lambda (note string finger)
      (let ((digit (if (null? finger)
                       #f
                       finger)))
        (if (and (null? string)
                 (not (eq? digit 0)))
            (set! unassigned-notes (cons note unassigned-notes))
            (if (eq? digit 0)
                (let ((fit-string
                      (find (lambda (string)
                              (open-string string (note-pitch note)))
                            free-strings)))
                  (if fit-string
                      (begin
                        (delete-free-string fit-string)
                        (set-fret note fit-string))
                      (begin
                        (ly:warning (_ "No open string for pitch ~a")
                                       (note-pitch note))
                        (set! unassigned-notes (cons note unassigned-notes)))))
                (begin
                  (delete-free-string string)
                  (set-fret note string))))))
    notes defined-strings defined-fingers)

  ;; handle notes without strings assigned
  (for-each
   (lambda (note)
     (let ((fit-string
            (find (lambda (string)
                    (string-qualifies string (note-pitch note)))
                  free-strings)))
        (if fit-string
            (set-fret note fit-string)
            (ly:warning (_ "No string for pitch ~a (given frets ~a)")
                           (note-pitch note)
                           specified-frets))))
   (sort unassigned-notes note-pitch>?))

   string-fret-fingering-tuples)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tablature

;; The TabNoteHead tablatureFormat callbacks.

;; Calculate the fret from pitch and string number as letter
;; The fret letter is taken from 'fretLabels if present
(define-public (fret-letter-tablature-format
                context string-number fret-number)
 (let ((labels (ly:context-property context 'fretLabels)))
  (make-vcenter-markup
   (cond
    ((= 0 (length labels))
     (string (integer->char (+ fret-number (char->integer #\a)))))
    ((and (<= 0 fret-number) (< fret-number (length labels)))
     (list-ref labels fret-number))
    (else
     (ly:warning (_ "No label for fret ~a (on string ~a);
only ~a fret labels provided")
                fret-number string-number (length labels))
     ".")))))

;; Display the fret number as a number
(define-public (fret-number-tablature-format
                context string-number fret-number)
  (make-vcenter-markup
    (format "~a" fret-number)))

;; The 5-string banjo has got a extra string, the fifth (duh), which
;; starts at the fifth fret on the neck.  Frets on the fifth string
;; are referred to relative to the other frets:
;;   the "first fret" on the fifth string is really the sixth fret
;;   on the banjo neck.
;; We solve this by defining a new fret-number-tablature function:
(define-public (fret-number-tablature-format-banjo
                context string-number fret-number)
 (make-vcenter-markup
  (number->string (cond
                   ((and (> fret-number 0) (= string-number 5))
                    (+ fret-number 5))
                   (else fret-number)))))

;;  Tab note head staff position functions
;;
;;  Define where in the staff to display a given string.  Some forms of
;;  tablature put the tab note heads in the spaces, rather than on the
;;  lines

(define-public (tablature-position-on-lines context string-number)
 (let* ((string-tunings (ly:context-property context 'stringTunings))
        (string-count (length string-tunings))
        (string-one-topmost (ly:context-property context 'stringOneTopmost))
        (staff-line (- (* 2 string-number) string-count 1)))
  (if string-one-topmost
      (- staff-line)
      staff-line)))

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
