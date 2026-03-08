;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2026 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define (conditional-string-capitalize str condition)
  (if condition
      str
      (string-capitalize str)))

(define*-public (pitch->name pitch #:optional language)
  "Return name of @var{pitch} without accidentals or octaves.

Optional argument @var{language} sets the language used to display
the pitch name (see file @file{define-note-names.scm} for
available values); if this symbol is missing or set to @code{#f},
the current input language is used."
  (let* ((lang (or language input-language))
         (pitch-alist
          (if (not language)
              pitchnames
              (assoc-get language
                         language-pitch-names '())))
         (note-name (ly:pitch-notename pitch))
         (alteration (ly:pitch-alteration pitch))
         (german-type (assq lang pitch-names-german-type))
         (type (and german-type (cdr german-type)))
         ;; Handle 'H', 'B', and 'Bb'.
         (result (if (and (= note-name 6)
                          (or (and (= alteration FLAT)
                                   (or (eq? type 'only-b)
                                       (eq? type 'only-b-flat)))
                              (and (<= alteration FLAT)
                                   (eq? type 'b-and-bes))))
                     '(b)
                     (rassoc pitch
                             (filter (lambda (p)
                                       (eq? (ly:pitch-alteration (cdr p)) 0))
                                     pitch-alist)
                             (lambda (a b)
                               (= (ly:pitch-notename a)
                                  (ly:pitch-notename b)))))))
    (if result (symbol->string (car result)))))

(define*-public (pitch->alteration pitch #:optional language)
  "Return the alteration of @var{pitch}.

Optional argument @var{language} sets the language used to display
the pitch name (see file @file{define-note-names.scm} for
available values); if this symbol is missing or set to @code{#f},
the current input language is used."
  (let* ((lang (or language input-language))
         (note-name (ly:pitch-notename pitch))
         (alteration (ly:pitch-alteration pitch))
         (german-type (assq lang pitch-names-german-type))
         (type (and german-type (cdr german-type))))
    ;; Handle 'H', 'B', and 'Bb'.
    (if (= note-name 6)
        (cond
         ((and (= alteration FLAT)
               (eq? type 'only-b))
          0)
         ((and (<= alteration FLAT)
               (eq? type 'b-and-bes))
          (- alteration FLAT))
         (else
          alteration))
        alteration)))

(define-public (accidental->text-markup alteration)
  "Return accidental glyph markup for @var{alteration}, to be used in text.

This is a simple wrapper around the @code{\\text-accidental}
markup function, providing smaller glyphs."
  (make-smaller-markup (make-text-accidental-markup alteration)))

(define (accidental->markup alteration)
  "A wrapper around 'accidental->text-markup', suppressing naturals."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (accidental->text-markup alteration)))

(define (accidental->markup-italian alteration)
  "Return accidental markup for ALTERATION, for use after an Italian
(or French) chord root name."
  (if (= alteration 0)
      (make-hspace-markup 0.2)
      (make-line-markup
       (list
        (make-hspace-markup 0.2)
        (accidental->markup alteration)
        (make-hspace-markup 0.1)))))

;;; Callback functions for `chordRootNamer` and `chordNoteNamer` context
;;; properties.  Both properties expect the same arguments:
;;;
;;;   (callback-func pitch lowercase?)
;;;
;;; Note that `chordNoteNamer` calls `callback-func` with `lowercase?` always
;;; set to `#f`.

(define-public ((chord-name:markup language) pitch lowercase?)
  "Return note name markup with accidental glyphs for @var{pitch}.

Argument @var{language} sets the language used to display the
pitch name (see file @file{define-note-names.scm} for available
values); if this symbol is missing or set to @code{#f}, the
current input language is used.

If @var{lowercase?} is not @code{#f}, a lowercase note name is
returned, otherwise the first character gets capitalized.

This is a callback function for either the @code{chordRootNamer}
or the @code{chordNoteNamer} context property."
  (let* ((str (pitch->name pitch language))
         (alt (pitch->alteration pitch language)))
    (make-line-markup
     (list (conditional-string-capitalize str lowercase?)
           (accidental->markup alt)))))

(define-public ((chord-name:german-markup german?) pitch lowercase?)
  "Return German note name markup with accidental glyphs for @var{pitch}.

This function displays pitch@tie{}B as letter@tie{}H.  If
@var{german?} is not @code{#f}, display pitch B-flat as
letter@tie{}B.  If equal to @code{#f}, display this pitch as
letter@tie{}B with a flat.

If @var{lowercase?} is not @code{#f}, a lowercase note name is
returned, otherwise the first character gets capitalized.

This is a callback function for either the @code{chordRootNamer}
or the @code{chordNoteNamer} context property."
  (let ((language (if german? 'deutsch 'semi-german)))
    ((chord-name:markup language) pitch lowercase?)))

(define-public ((chord-name:name-markup language) pitch lowercase?)
  "Return note name markup for @var{pitch}.

If @var{lowercase?} is not @code{#f}, a lowercase note name is
returned, otherwise the first character gets capitalized.

Use function @code{pitch->name} to get a pitch name without
accidentals.

Argument @var{language} sets the language used to display the
pitch name; if this symbol is missing or set to @code{#f}, the
current input language is used.

This is a callback function for either the @code{chordRootNamer}
or the @code{chordNoteNamer} context property."
  (define (pitch= pitch1 pitch2)
    (and (= (ly:pitch-notename pitch1) (ly:pitch-notename pitch2))
         (= (ly:pitch-alteration pitch1) (ly:pitch-alteration pitch2))))

  (let* ((pitch-alist
          (if (not language)
              pitchnames
              (assoc-get language
                         language-pitch-names '())))
         (result (rassoc pitch pitch-alist pitch=)))
    (when result
      (conditional-string-capitalize (symbol->string (car result))
                                     lowercase?))))

(define-public (chord-name:german-lowercase-name-markup pitch lowercase?)
  "Return German lowercase note name markup for @var{pitch}.

Argument @var{lowercase?} is ignored."
  ((chord-name:name-markup 'deutsch) pitch #t))

(define-public ((chord-name:italian-markup french?) pitch lowercase?)
  "Return Italian note name markup with accidental glyphs for @var{pitch}.

If @var{french?} is not @code{#f}, French note names are
returned (for example, @q{ré} instead of @q{re} for pitch@tie{}D).

If @var{lowercase?} is not @code{#f}, a lowercase note name is
returned, otherwise the first character gets capitalized.

This is a callback function for either the @code{chordRootNamer}
or the @code{chordNoteNamer} context property."
  (let* ((name (pitch->name pitch
                            (if french? 'français 'italiano)))
         (alt (ly:pitch-alteration pitch)))
    (make-line-markup
     (list
      (conditional-string-capitalize name lowercase?)
      (accidental->markup-italian alt)))))

(define*-public (sequential-music-to-chord-exceptions seq #:optional omit-root)
  "Transform sequential music @var{seq} to chord exceptions.

@var{seq} is music that contains a sequence of chords with
attached markup having the form

@example
<@var{pitch1} @var{pitch2} ... >...-\\markup @{ @var{markup} @}
@end example

@noindent
for example, @code{<c e g b d'>-\\markup \\super \"maj9\"}.

Each chord gets transformed to a chord exception, which is a
two-element list: its first element is a list representing the
pitches @var{pitch1}, @var{pitch2}, etc., in a normalized form;
the second element holds a procedure that generates @var{markup}.

If optional argument @var{omit-root} is set and not equal to
@code{#f}, @var{pitch1} (i.e., the root of the chord) is omitted
while constructing the chord exception.

The function returns a list of all chord exceptions given in
@var{seq}."
  (define (chord-to-exception-entry m)
    (let* ((elts (ly:music-property m 'elements))
           (pitches (map (lambda (x) (ly:music-property x 'pitch))
                         (filter
                          (lambda (y) (memq 'note-event
                                            (ly:music-property y 'types)))
                          elts)))
           (sorted (sort pitches ly:pitch<?))
           (root (car sorted))
           (normalized (map (lambda (x) (- x root)) sorted))
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
