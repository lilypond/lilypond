;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules ((ice-9 list)
              #:select (rassoc)))

(define (natural-chord-alteration p)
  "Return the natural alteration for step P."
  (if (= (ly:pitch-steps p) 6)
      FLAT
      0))

(define (conditional-string-capitalize str condition)
  (if condition
      str
      (string-capitalize str)))

;;
;; TODO: make into markup.
;;

;; flat and flat-based glyphs have a lesser Y-extent than sharps.
(define (short-glyph? alteration)
  (< alteration 0))

;; natural, single flat and mirrored flat have a narrower X-extent.
(define (narrow-glyph? alteration)
  (member alteration
          ;; flat-based glyphs in various notation systems.
          ;; (duplicates come from the input files, for
          ;; consistency's sake) -vv
          '(0
            ;; western accidentals
            -1/2 -1/4
            ;; makam
            -1/9 -5/18 -6/18 -4/9 -5/9 -8/9
            ;; turkish makam
            -1/12 -3/12 -4/12 -5/12 -6/12 -10/12
            ;; hel-arabic
            -1/4 -3/4 ; -5/2 -> flatflat, ignored
            -7/2)))

(define-public (alteration->text-accidental-markup alteration)

  (make-smaller-markup
   (make-raise-markup
    (if (short-glyph? alteration)
        0.3
        0.6)
    (make-accidental-markup alteration))))

(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (conditional-kern-before
       (alteration->text-accidental-markup alteration)
       (narrow-glyph? alteration) 0.094725)))

(define (accidental->markup-italian alteration)
  "Return accidental markup for ALTERATION, for use after an italian chord root name."
  (if (= alteration 0)
      (make-hspace-markup 0.2)
      (make-line-markup
       (list
        ;; FIXME -- see issue 3330.
        (make-hspace-markup (if (= alteration FLAT) 0.57285385 0.5))
        (make-raise-markup 0.7 (alteration->text-accidental-markup alteration))
        (make-hspace-markup (if (= alteration SHARP) 0.2 0.1))
        ))))

(define-public (note-name->string pitch . language)
  "Return pitch string for @var{pitch}, without accidentals or octaves.
Current input language is used for pitch names, except if an
other @var{language} is specified."
  ;; See also note-name->lily-string if accidentals are needed.
  (let* ((pitch-alist
          (if (null? language) pitchnames
              (assoc-get (car language)
                         language-pitch-names '())))
         (result (rassoc pitch
                         (filter  (lambda (p)
                                    ;; TODO: add exception for German B?
                                    (eq? (ly:pitch-alteration (cdr p)) 0))
                                  pitch-alist)
                         (lambda (a b)
                           (= (ly:pitch-notename a)
                              (ly:pitch-notename b))))))
    (if result (symbol->string (car result)))))

(define-public (note-name->markup pitch lowercase?)
  "Return pitch markup for @var{pitch}, including accidentals
printed as glyphs.  If @var{lowercase?} is set to false, the
note names are capitalized."
  (let ((str (note-name->string pitch)))
    (make-line-markup
     (list
      (make-simple-markup
       (conditional-string-capitalize str lowercase?))
      (accidental->markup (ly:pitch-alteration pitch))))))

(define (pitch-alteration-semitones pitch)
  (inexact->exact (round (* (ly:pitch-alteration pitch) 2))))

(define-public ((chord-name->german-markup B-instead-of-Bb)
                pitch lowercase?)
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
       (conditional-string-capitalize
        ;; TODO: use note-name->string with an exception for B.
        (vector-ref #("c" "d" "e" "f" "g" "a" "h" "b") (car n-a))
        lowercase?))
      (accidental->markup (/ (cdr n-a) 2))))))

(define-public (note-name->german-markup pitch lowercase?)
  ;; TODO: rewrite using note-name->lily-string.
  ;; FIXME: lowercase? is ignored.
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

(define-public ((chord-name->italian-markup french?) pitch lowercase?)
  "Return pitch markup for @var{pitch}, using Italian/@/French note names.
If @var{french?} is set to @code{#t}, french `ré' is returned for
pitch@tie{}D instead of `re'."

  (let* ((name (note-name->string pitch
                                  (if french? 'français 'italiano)))
         (alt (ly:pitch-alteration pitch)))
    (make-line-markup
     (list
      (make-simple-markup
       (conditional-string-capitalize name lowercase?))
      (accidental->markup-italian alt)))))

;; fixme we should standardize on omit-root (or the other one.)
;; perhaps the default should also be reversed --hwn
(define-public (sequential-music-to-chord-exceptions seq . rest)
  "Transform sequential music @var{seq} of type

@example
@code{<<c d e>>-\\markup@{ foobar @}}
@end example

to @code{(cons @var{cde-pitches} @var{foobar-markup})}, or to
@code{(cons @var{de-pitches} @var{foobar-markup})} if @var{omit-root} is given
and non-false."

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
    (filter cdr alist)))
