;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
     (assoc-get alteration standard-alteration-glyph-name-alist "")))))
  
(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (conditional-kern-before
       (alteration->text-accidental-markup alteration)
       (= alteration FLAT) 0.2)))

(define (accidental->markup-italian alteration)
  "Return accidental markup for ALTERATION, for use after an italian chord root name."
  (if (= alteration 0)
      (make-hspace-markup 0.2)
      (make-line-markup
       (list
        (make-hspace-markup (if (= alteration FLAT) 0.7 0.5))
	(make-raise-markup 0.7 (alteration->text-accidental-markup alteration))
	(make-hspace-markup (if (= alteration SHARP) 0.2 0.1))
	))))

(define-public (note-name->markup pitch)
  "Return pitch markup for PITCH."
  (make-line-markup
   (list
    (make-simple-markup
     (vector-ref #("C" "D" "E" "F" "G" "A" "B") (ly:pitch-notename pitch)))
     (accidental->markup (ly:pitch-alteration pitch)))))

(define (pitch-alteration-semitones pitch)
  (inexact->exact (round (* (ly:pitch-alteration pitch) 2))))

(define-safe-public ((chord-name->german-markup B-instead-of-Bb) pitch)
  "Return pitch markup for PITCH, using german note names.
   If B-instead-of-Bb is set to #t real german names are returned.
   Otherwise semi-german names (with Bb and below keeping the british names)
"
  (let* ((name (ly:pitch-notename pitch))
         (alt-semitones  (pitch-alteration-semitones pitch))
	 (n-a (if (member (cons name alt-semitones) `((6 . -1) (6 . -2)))
		  (cons 7 (+ (if B-instead-of-Bb 1 0) alt-semitones))
		  (cons name alt-semitones))))
    (make-line-markup
     (list
      (make-simple-markup
       (vector-ref #("C" "D" "E" "F" "G" "A" "H" "B") (car n-a)))
      (make-normal-size-super-markup
       (accidental->markup (/ (cdr n-a) 2)))))))

(define-safe-public (note-name->german-markup pitch)
  (let* ((name (ly:pitch-notename pitch))
	 (alt-semitones (pitch-alteration-semitones pitch))
	 (n-a (if (member (cons name alt-semitones) `((6 . -1) (6 . -2)))
		  (cons 7 (+ 1 alt-semitones))
		  (cons name alt-semitones))))
    (make-line-markup
     (list
      (string-append
       (list-ref '("c" "d" "e" "f" "g" "a" "h" "b") (car n-a))
       (if (or (equal? (car n-a) 2) (equal? (car n-a) 5))
	   (list-ref '( "ses" "s" "" "is" "isis") (+ 2 (cdr n-a)))
	   (list-ref '("eses" "es" "" "is" "isis") (+ 2 (cdr n-a)))))))))

(define-public ((chord-name->italian-markup re-with-eacute) pitch)
  "Return pitch markup for PITCH, using italian/french note names.
   If re-with-eacute is set to #t, french 'ré' is returned for D instead of 're'
"
  (let* ((name (ly:pitch-notename pitch))
         (alt (ly:pitch-alteration pitch)))
    (make-line-markup
     (list
      (make-simple-markup
       (vector-ref
        (if re-with-eacute
            #("Do" "Ré" "Mi" "Fa" "Sol" "La" "Si")
            #("Do" "Re" "Mi" "Fa" "Sol" "La" "Si"))
        name))
      (accidental->markup-italian alt)
      ))))

;; fixme we should standardize on omit-root (or the other one.)
;; perhaps the default should also be reversed --hwn
(define-safe-public (sequential-music-to-chord-exceptions seq . rest)
  "Transform sequential music SEQ of type <<c d e>>-\\markup{ foobar }
to (cons CDE-PITCHES FOOBAR-MARKUP), or to (cons DE-PITCHES
FOOBAR-MARKUP) if OMIT-ROOT is given and non-false.
"

  (define (chord-to-exception-entry m)
    (let* ((elts (ly:music-property m 'elements))
	   (omit-root (and (pair? rest) (car rest)))
	   (pitches (map (lambda (x) (ly:music-property x 'pitch))
			 (filter
			  (lambda (y) (memq 'note-event
					    (ly:music-property y 'types)))
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
	   (texts (map (lambda (x) (ly:music-property x 'text))
		       (filter
			(lambda (y) (memq 'text-script-event
					  (ly:music-property y 'types)))
			elts)))

	   (text (if (null? texts) #f (if omit-root (car texts) texts))))
      (cons (if omit-root (cdr normalized) normalized) text)))

  (define (is-event-chord? m)
    (and
     (memq 'event-chord (ly:music-property m 'types))
     (not (equal? ZERO-MOMENT (ly:music-length m)))))

  (let* ((elts (filter is-event-chord? (ly:music-property seq 'elements)))
	 (alist (map chord-to-exception-entry elts)))
    (filter (lambda (x) (cdr x)) alist)))

