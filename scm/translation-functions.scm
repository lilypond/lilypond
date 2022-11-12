;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (c) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
;;;;                          Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules (srfi srfi-2)) ;; for `and-let*`


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; caesura

(define-public ((at-bar-line-substitute-caesura-type substitute-type)
                context caesura-type observations)
  "At a bar line, create the caesura using @var{substitute-type} rather
than the value of @code{caesuraType}."
  (if (memq 'bar-line observations)
      (let ((arts (assq-ref caesura-type 'articulations)))
        (acons 'articulations arts substitute-type))
      caesura-type))

(define-public (caesura-to-divisio context caesura-type observations)
  "@code{caesuraTypeTransform} callback to print articulated caesurae as
chant breath marks."
  (let ((arts (assq-ref caesura-type 'articulations)))
    (set! caesura-type
          (cond
           ((memq 'fermata arts)
            `((articulations . ,(delq 'fermata arts))
              (breath . chantfullbar)))
           ((memq 'shortfermata arts)
            `((articulations . ,(delq 'shortfermata arts))
              (breath . chanthalfbar)))
           (else
            caesura-type))))
  ;; TODO: This transformation is most likely to be used when
  ;; forbidBreakBetweenBarLines is false, but to improve the other
  ;; case, we could set (underlying-bar-line . "") here to allow a
  ;; break at any caesura.  This trick would not be suitable for
  ;; contexts where an an underlying repeat bar might need to appear
  ;; -- the caesura bar would take precedence -- but that shouldn't be
  ;; a problem in the ancient contexts for which this was written.
  caesura-type)

(define-public (caesura-to-bar-line-or-divisio
                context caesura-type observations)
  "@code{caesuraTypeTransform} callback to print articulated caesurae as
chant breath marks using the infrastructure for modern bar lines when
possible."
  (set! caesura-type (caesura-to-divisio context caesura-type observations))
  (let ((arts (assq-ref caesura-type 'articulations)))
    (case (assq-ref caesura-type 'breath)
      ((chantdoublebar)
       `((articulations . ,arts)
         (bar-line . "||")))
      ((chantfullbar)
       `((articulations . ,arts)
         (bar-line . "|")))
      ((chanthalfbar)
       `((articulations . ,arts)
         (bar-line . ",")))
      ((chantquarterbar)
       `((articulations . ,arts)
         (bar-line . "'")))
      (else caesura-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clefs

(define-public (clef-transposition-markup oct style)
  "The transposition sign formatting function.  @var{oct} is supposed to be
a string holding the transposition number, @var{style} determines the
way the transposition number is displayed."
  (let* ((delim (if (symbol? style)
                    (case style
                      ((parenthesized) (cons "(" ")"))
                      ((bracketed) (cons "[" "]"))
                      (else (cons "" "")))
                    (cons "" "")))
         (text (string-concatenate (list (car delim) oct (cdr delim)))))

    (make-vcenter-markup text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metronome marks

(define-public (format-metronome-markup event context)
  (let ((hide-note (ly:context-property context 'tempoHideNote #f))
        (text (ly:event-property event 'text))
        (dur (ly:event-property event 'tempo-unit))
        (count (ly:event-property event 'metronome-count)))
    (metronome-markup text dur count hide-note)))

(define (metronome-markup text dur count hide-note)
  (let* ((note-mark
          (if (and (not hide-note) (ly:duration? dur))
              (make-smaller-markup
               (make-note-by-number-markup
                (ly:duration-log dur)
                (ly:duration-dot-count dur)
                UP))
              #f))
         (count-markup (cond ((number? count)
                              (if (> count 0)
                                  (make-simple-markup
                                   (number->string count))
                                  #f))
                             ((pair? count)
                              ;; Thin Spaces U+2009 & En-dash U+2013
                              (make-simple-markup
                               (format #f "~a – ~a" (car count) (cdr count))))
                             (else #f)))
         (note-markup (if (and (not hide-note) count-markup)
                          (list
                           (make-general-align-markup Y DOWN note-mark)
                           (make-simple-markup " = ")
                           count-markup)
                          #f))
         (text-markup (if (not (null? text)) (make-bold-markup text) #f)))
    (if text-markup
        (if (and note-markup (not hide-note))
            (make-line-markup (list (make-concat-markup
                                     (append (list text-markup
                                                   (make-simple-markup " ("))
                                             note-markup
                                             (list (make-simple-markup ")"))))))
            (make-line-markup (list text-markup)))
        (if note-markup
            (make-line-markup (list (make-concat-markup note-markup)))
            (make-null-markup)))))

(define (select-option options available)
  (let ((o (find (lambda (x) (memq x available))
                 options)))
    (if o o (car available))))

(define-public (format-dal-segno-text
                context
                return-count
                marks)
  (let* ((start-markup (car marks))
         (end-markup (cadr marks))
         (next-markup (caddr marks))
         (result (if start-markup "D.S." "D.C.")))
    (if start-markup
        (set! result (make-line-markup (list result start-markup))))
    (if (= 1 return-count)
        ;; The "al (mark)" detail would probably do more harm than
        ;; good when there are multiple returns, since it matters the
        ;; last time only.
        (if (and next-markup end-markup) ; "... al (mark) e poi ..."
            (set! result (make-line-markup (list result "al" end-markup))))
        (set! result (make-line-markup
                      (list result (number->string return-count) "V."))))
    (if next-markup ; "... e poi ..."
        (set! result (make-right-column-markup
                      (list
                       result
                       (make-line-markup (list "e poi la" next-markup)))))
        (if end-markup ; like "... al Fine"
            (set! result (make-right-column-markup
                          (list
                           result
                           (make-line-markup (list "al" end-markup)))))))
    result))

(define-public (format-dal-segno-text-brief
                context
                return-count
                marks)
  (let* ((start-markup (car marks))
         (end-markup (cadr marks))
         (next-markup (caddr marks))
         (result (if start-markup "D.S." "D.C.")))
    (if start-markup
        (set! result (make-line-markup (list result start-markup))))
    (if (!= 1 return-count)
        (set! result (make-line-markup
                      (list result (number->string return-count) "V."))))
    (if next-markup ; like "al Coda"
        (set! result (make-right-column-markup
                      (list
                       result
                       (make-line-markup (list "al" next-markup)))))
        (if end-markup ; like "al Fine"
            (set! result (make-right-column-markup
                          (list
                           result
                           (make-line-markup (list "al" end-markup)))))))
    result))

(define-public (make-lyric-repeat-count-formatter options)
  ;; Appending 'medium to the options defeats the default 'bold of
  ;; format-mark-generic while still allowing the user to request
  ;; bold.
  (let ((mark-formatter (format-mark-generic (append options '(medium)))))
    (lambda (context repeat-count)
      (if (positive? repeat-count)
          (mark-formatter repeat-count context)))))

(define-public (format-mark-generic options)
  ;; Select "alphabet", frame, font-series, letter-case and double letter behaviour
  ;; from options list; if none is given, default to first available.
  (let ((ab (select-option options '(alphabet-omit-i alphabet alphabet-omit-j barnumbers numbers roman roman-ij)))
        (fr (select-option options '(noframe box circle oval)))
        (fs (select-option options '(bold medium)))
        (lc (select-option options '(uppercase lowercase mixedcase)))
        (dl (select-option options '(combine repeat)))
        (dot (select-option options '(nodot dot)))
        (sign (select-option options '(nosign coda segno varcoda))))
    (lambda (number context)
      (let* ((the-string
              (case ab
                ((barnumbers) (number->string (ly:context-property context 'currentBarNumber)))
                ((numbers) (number->string number))
                ((roman) (number-format 'roman-upper number))
                ((roman-ij) (number-format 'roman-ij-upper number))
                (else (markgeneric-string number ab dl))))
             (the-cased-string
              (case lc
                ;; both roman numbers and alphabet-based marks are
                ;; already uppercase, (bar)numbers aren't affected
                ((uppercase)                    the-string)
                ((mixedcase) (string-capitalize the-string))
                ((lowercase) (string-downcase   the-string))))
             (the-dotted-string
              (case dot
                ((dot) (string-append the-cased-string "."))
                (else the-cased-string)))
             (the-flanked-string
              (case sign
                ((coda) (format-sign-with-number
                         number (make-coda-markup) the-dotted-string))
                ((segno) (format-sign-with-number
                          number (make-segno-markup) the-dotted-string))
                ((varcoda) (format-sign-with-number
                            number (make-varcoda-markup) the-dotted-string))
                (else the-dotted-string)))
             (the-framed-string
              (case fr
                ((box)    (make-box-markup    the-flanked-string))
                ((circle) (make-circle-markup the-flanked-string))
                ((oval)   (make-oval-markup   the-flanked-string))
                ((noframe)                    the-flanked-string))))
        (case fs
          ((bold) (make-bold-markup the-framed-string))
          ((medium)                 the-framed-string))))))

(define-public format-coda-mark
  (format-mark-generic '(numbers coda)))

(define-public format-mark-alphabet
  (format-mark-generic '(alphabet)))

(define-public format-mark-box-alphabet
  (format-mark-generic '(alphabet box)))

(define-public format-mark-circle-alphabet
  (format-mark-generic '(alphabet circle)))

(define-public format-mark-letters
  (format-mark-generic '()))

(define-public format-mark-numbers
  (format-mark-generic '(numbers)))

(define-public format-mark-barnumbers
  (format-mark-generic '(barnumbers)))

(define-public format-mark-box-letters
  (format-mark-generic '(box)))

(define-public format-mark-circle-letters
  (format-mark-generic '(circle)))

(define-public format-mark-box-numbers
  (format-mark-generic '(numbers box)))

(define-public format-mark-circle-numbers
  (format-mark-generic '(numbers circle)))

(define-public format-mark-box-barnumbers
  (format-mark-generic '(barnumbers box)))

(define-public format-mark-circle-barnumbers
  (format-mark-generic '(barnumbers circle)))

(define-public format-segno-mark
  (format-mark-generic '(numbers segno)))

(define-public (format-segno-mark-considering-bar-lines segno-number context)
  "When bar lines incorporate segni, print no mark for the first segno
because that would be redundant.  Print the usual marks for later
segni to avoid ambiguity."
  (if (and
       (<= segno-number 1)
       (eq? (ly:context-property context 'segnoStyle) 'bar-line))
      empty-markup
      (format-segno-mark segno-number context)))

(define (format-sign-with-number number sign-markup number-markup)
  (make-concat-markup
   (if (<= number 2)
       (make-list number sign-markup)
       (list sign-markup
             (ly:wide-char->utf-8 #x200a) ;; Unicode hair space
             number-markup
             (ly:wide-char->utf-8 #x200a)
             sign-markup))))

(define-public format-varcoda-mark
  (format-mark-generic '(numbers varcoda)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bass figures.

(define figbass-accidental-alist
  '((-1 . #x1d12b)
    (-1/2 . #x266d)
    (0 . #x266e)
    (1/2 . #x266f)
    (1 . #x1d12a)))

(define-public (format-bass-figure figure event context)
  (let* (;; User properties controlling the figured bass layout.
         (large-number-alignment
          (ly:context-property context 'figuredBassLargeNumberAlignment))
         (figbass-alist
          (ly:context-property context 'figuredBassPlusStrokedAlist))
         (alt-dir
          (ly:context-property context 'figuredBassAlterationDirection))
         (plus-dir
          (ly:context-property context 'figuredBassPlusDirection))

         (augmented (ly:event-property event 'augmented))

         ;; The digit(s), horizontally positioned, or #f.
         (fig-markup
          (if (number? figure)
              ((if (<= 10 figure)
                   (lambda (y)
                     (make-align-on-other-markup
                      X
                      large-number-alignment (make-number-markup "1")
                      large-number-alignment y))
                   identity)
               (cond
                ((eq? #t (ly:event-property event 'diminished))
                 (make-slashed-digit-markup figure))
                ((eq? #t (ly:event-property event 'augmented-slash))
                 ;; Use specially stroked digit if available and wanted.
                 (or (and-let* (((<= 6 figure 9))
                                (glyph (assv-ref figbass-alist figure)))
                       (make-musicglyph-markup glyph))
                     (make-backslashed-digit-markup figure)))
                ((eq? #t augmented)
                 ;; Use special digit with plus if available and wanted.
                 (or (and-let* (((>= 5 figure 2))
                                ((eqv? plus-dir RIGHT))
                                (glyph (assv-ref figbass-alist figure)))
                       (set! augmented #f)
                       (make-musicglyph-markup glyph))
                     (make-number-markup (number->string figure 10))))
                (else (make-number-markup (number->string figure 10)))))
              #f))

         (alt (ly:event-property event 'alteration))
         (alt-bracket (ly:event-property event 'alteration-bracket #f))
         ;; The alteration, probably bracketed but not positioned yet,
         ;; or #f.
         (alt-markup
          (if (number? alt)
              ((if alt-bracket make-bracket-markup identity)
               (make-number-markup
                (ly:wide-char->utf-8
                 (or
                  (assv-ref figbass-accidental-alist alt)
                  (begin
                    (ly:warning
                     (G_ "no accidental glyph found for alteration ~a")
                     alteration)
                    #\?)))))
              #f))

         (plus-markup (if (eq? #t augmented)
                          (make-number-markup "+")
                          #f)))

    (if (and (not alt-markup) alt-bracket)
        (ly:programming-error
         "Cannot put brackets around non-existent bass figure alteration."))

    ;; We treat a solitary alteration similarly to digits.
    (if (and (not fig-markup) alt-markup)
        (begin
          (set! fig-markup
                (make-align-on-other-markup
                 X
                 CENTER (make-number-markup "1")
                 CENTER alt-markup))
          (set! alt-markup #f)))

    ;; We treat a solitary plus similarly to digits (but enlarged).
    (if (and (not fig-markup) plus-markup)
        (begin
          (set! fig-markup
                (make-align-on-other-markup
                 Y
                 CENTER (make-number-markup "1")
                 CENTER (make-align-on-other-markup
                         X
                         CENTER (make-number-markup "1")
                         CENTER (make-fontsize-markup 3 plus-markup))))
          (set! plus-markup #f)))

    ;; The alteration gets attached either to the left or the right of
    ;; the digit(s).
    (if alt-markup
        (set! fig-markup
              (make-put-adjacent-markup
               X (if (number? alt-dir)
                     alt-dir
                     LEFT)
               fig-markup
               (make-pad-x-markup 0.1 alt-markup))))

    ;; Ditto for the plus mark.
    (if plus-markup
        (set! fig-markup
              (if fig-markup
                  (make-put-adjacent-markup
                   X (if (number? plus-dir)
                         plus-dir
                         LEFT)
                   fig-markup plus-markup)
                  plus-markup)))

    (if (markup? fig-markup)
        (make-fontsize-markup -5 fig-markup)
        (make-null-markup))))

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
  (determine-string-fret-finger context notes specified-info rest)
  "Determine string numbers and frets for playing @var{notes}
as a chord, given specified information @var{specified-info}.
@var{specified-info} is a list with two list elements,
specified strings @code{defined-strings} and
specified fingerings @code{defined-fingers}.  Only a fingering of@tie{}0
will affect the fret selection, as it specifies an open string.
If @code{defined-strings} is @code{'()}, the context property
@code{defaultStrings} is used as a list of defined strings.
Looks for predefined fretboards if @code{predefinedFretboardTable}
is not @code{#f}.  If @var{rest} is present, it contains the
@code{FretBoard} grob, and a fretboard gets
created.  Otherwise, a list of @code{(string fret finger)} lists
is returned.

If the context-property @code{supportNonIntegerFret} is set @code{#t},
micro-tones are supported for @code{TabStaff}, but not not for
@code{FretBoards}."

  ;;  helper functions
  (define (barre-list string-frets)
    "Create a barre-list that reflects the string, fret, and finger
entries in @var{string-frets}."
    (let* ((finger-range '(0  4)) ; range of possible finger numbers
           (barre-elements 4) ; 4 elements per barre entry:
                                        ; 'barre
                                        ; highest string number
                                        ; lowest string number
                                        ; finger (seems redundant, but makes it
                                        ;  easy to convert from array to list
           (barres (make-array 0 finger-range barre-elements))
           (add-string-fret
            (lambda(sf)
              (let ((string (car sf))
                    (fret (cadr sf))
                    (finger (if (null? (caddr sf)) 0 (caddr sf))))
                (if (and (not (= fret 0)) (not (= finger 0)))
                    (begin
                      (array-set! barres 'barre finger 0)
                      (array-set! barres fret finger 3)
                      (if (or (< (array-ref barres finger 1) string)
                              (= 0 (array-ref barres finger 1)))
                          (array-set! barres string finger 1))
                      (if (or (> (array-ref barres finger 2) string)
                              (= 0 (array-ref barres finger 2)))
                          (array-set! barres string finger 2)))))))
           (barre-list (begin
                         (map add-string-fret string-frets)
                         (array->list barres))))
      (filter (lambda(l) (and (eq? (car l) 'barre)
                              (not (= (fourth l) 0))
                              (> (second l) (third l))))
              barre-list)))


  (define (string-frets->placement-list string-frets string-count)
    "Convert @var{string-frets} to @code{fret-diagram-verbose}
dot placement entries."
    (let* ((placements (list->vector
                        (map (lambda (x) (list 'mute  x))
                             (iota string-count 1))))
           (no-fingers (null? (filter (lambda (sf)
                                        (not (null? (caddr sf))))
                                      string-frets)))
           (b-list (barre-list string-frets)))
      (for-each (lambda (sf)
                  (let* ((string (car sf))
                         (fret (cadr sf))
                         (finger (caddr sf)))
                    (vector-set!
                     placements
                     (1- string)
                     (if (= 0 fret)
                         (list 'open string)
                         (if (null? finger)
                             (list 'place-fret string fret)
                             (list 'place-fret string fret finger))))))
                string-frets)
      (if (or (null? b-list) no-fingers)
          (vector->list placements)
          (append (vector->list placements) b-list))))

  (define (placement-list->string-frets placement-list)
    "Convert @var{placement-list} to string-fret list."
    (map (lambda (x) (if (eq? (car x) 'place-fret)
                         (cdr x)
                         (list (cadr x) 0)))
         (filter (lambda (l) (or (eq? (car l) 'place-fret)
                                 (eq? (car l) 'open)))
                 placement-list)))

  (define (entry-count art-list)
    "Count the number of entries in a list of articulations."
    (length (filter (lambda (x) (not (null? x)))
                    art-list)))

  (define (string-number event)
    "Get the string-number from @var{event}.  Return @var{#f}
if no string-number is present."
    (let ((num (ly:event-property event 'string-number)))
      (and (integer? num) (positive? num) num)))

  (define (determine-frets-and-strings
           notes
           defined-strings
           defined-fingers
           minimum-fret
           maximum-stretch
           tuning)
    "Determine the frets and strings used to play the notes in
@var{notes}, given @var{defined-strings} and @var{defined-fingers}
along with @var{minimum-fret}, @var{maximum-stretch}, and
@var{tuning}.  Returns a list of @code{(string fret finger) lists."


    (define restrain-open-strings (ly:context-property context
                                                       'restrainOpenStrings
                                                       #f))
    (define specified-frets '())
    (define free-strings (iota (length tuning) 1))

    (define (calc-fret pitch string tuning)
      "Calculate the fret to play @var{pitch} on @var{string} with
@var{tuning}."
      (* 2  (- (ly:pitch-tones pitch) (ly:pitch-tones (list-ref tuning (1- string))))))

    (define (note-pitch note)
      "Get the pitch (in semitones) from @var{note}."
      (ly:event-property note 'pitch))

    (define (note-finger ev)
      "Get the fingering from @var{ev}.  Return @var{#f}
if no fingering is present."
      (let* ((articulations (ly:event-property ev 'articulations))
             (finger-found #f))
        (for-each (lambda (art)
                    (let* ((num (ly:event-property art 'digit)))

                      (if (and (ly:in-event-class? art 'fingering-event)
                               (number? num)
                               (> num 0))
                          (set! finger-found num))))
                  articulations)
        finger-found))

    (define (delete-free-string string)
      (if (number? string)
          (set! free-strings
                (delete string free-strings))))

    (define (close-enough fret)
      "Decide if @var{fret} is acceptable, given the already used frets."
      (every (lambda (specced-fret)
               (or (zero? specced-fret)
                   (zero? fret)
                   (>= maximum-stretch (abs (- fret specced-fret)))))
             specified-frets))

    (define (string-qualifies string pitch)
      "Can @var{pitch} be played on @var{string}, given already placed
notes?"
      (let* ((fret (calc-fret pitch string tuning)))
        (and (or (and (not restrain-open-strings)
                      (zero? fret))
                 (>= fret minimum-fret))
             (if (and
                  (ly:context-property context 'supportNonIntegerFret #f)
                  (null? rest))
                 (integer? (truncate fret))
                 (integer? fret))
             (close-enough fret))))

    (define (open-string string pitch)
      "Is @var{pitch} and open-string note on @var{string}, given
the current tuning?"
      (let* ((fret (calc-fret pitch string tuning)))
        (zero? fret)))

    (define (set-fret! pitch-entry string finger)
      (let ((this-fret (calc-fret (car pitch-entry)
                                  string
                                  tuning)))
        (if (< this-fret 0)
            (ly:warning (G_ "Negative fret for pitch ~a on string ~a")
                        (car pitch-entry) string)
            (if (and
                 (not (integer? this-fret))
                 (not (ly:context-property context 'supportNonIntegerFret #f)))
                (ly:warning (G_ "Missing fret for pitch ~a on string ~a")
                            (car pitch-entry) string)))
        (if (and (= this-fret 0)
                 (and finger
                      (not (null? finger))))
            (ly:warning (G_ "Open fret on string ~a has finger of ~a")
                        string finger))
        (delete-free-string string)
        (set! specified-frets (cons this-fret specified-frets))
        (list-set! string-fret-fingers
                   (cdr pitch-entry)
                   (list string this-fret finger))))

    (define (kill-note! string-fret-fingers note-index)
      (list-set! string-fret-fingers note-index (list #f #t)))

    (define string-fret-fingers
      (map (lambda (string finger)
             (if (null? finger)
                 (list string #f)
                 (list string #f finger)))
           defined-strings defined-fingers))

    ;;; body of determine-frets-and-strings
    (let* ((pitches (map note-pitch notes))
           (pitch-alist (map cons pitches (iota (length pitches)))))

      ;; handle notes with strings assigned and fingering of 0
      (for-each
       (lambda (pitch-entry string-fret-finger)
         (let* ((string (list-ref string-fret-finger 0))
                (finger (if (= (length string-fret-finger) 3)
                            (list-ref string-fret-finger 2)
                            '()))
                (pitch (car pitch-entry))
                (digit (if (null? finger)
                           #f
                           finger)))
           (if (or (not (null? string))
                   (eqv? digit 0))
               (if (eqv? digit 0)
                   ;; here we handle fingers of 0 -- open strings
                   (let ((fit-string
                          (find (lambda (string)
                                  (open-string string pitch))
                                free-strings)))
                     (if fit-string
                         (set-fret! pitch-entry fit-string #f)
                         (ly:warning (G_ "No open string for pitch ~a")
                                     pitch)))
                   ;; here we handle assigned strings
                   (let* ((this-fret
                           (calc-fret pitch string tuning))
                          (possible-fret?
                           (and (>= this-fret 0)
                                (if (and
                                     (ly:context-property
                                      context 'supportNonIntegerFret #f)
                                     (null? rest))
                                    (integer? (truncate this-fret))
                                    (integer? this-fret))))
                          (handle-negative
                           (ly:context-property context
                                                'handleNegativeFrets
                                                'recalculate)))
                     (cond ((or possible-fret?
                                (eq? handle-negative 'include))
                            (set-fret! pitch-entry string finger))
                           ((eq? handle-negative 'recalculate)
                            (begin
                              (ly:warning
                               (G_ "Requested string for pitch requires negative fret: string ~a pitch ~a")
                               string
                               pitch)
                              (ly:warning (G_ "Ignoring string request and recalculating."))
                              (list-set! string-fret-fingers
                                         (cdr pitch-entry)
                                         (if (null? finger)
                                             (list '() #f)
                                             (list '() #f finger)))))
                           ((eq? handle-negative 'ignore)
                            (begin
                              (ly:warning
                               (G_ "Requested string for pitch requires negative fret: string ~a pitch ~a")
                               string
                               pitch)
                              (ly:warning (G_ "Ignoring note in tablature."))
                              (kill-note! string-fret-fingers
                                          (cdr pitch-entry))))))))))
       pitch-alist string-fret-fingers)
      ;; handle notes without strings assigned -- sorted by pitch, so
      ;; we need to use the alist to have the note number available
      (for-each
       (lambda (pitch-entry)
         (let* ((string-fret-finger (list-ref string-fret-fingers
                                              (cdr pitch-entry)))
                (string (list-ref string-fret-finger 0))
                (finger (if (= (length string-fret-finger) 3)
                            (list-ref string-fret-finger 2)
                            '()))
                (pitch (car pitch-entry))
                (fit-string
                 (find (lambda (string)
                         (string-qualifies string pitch))
                       free-strings)))
           (if (not (list-ref string-fret-finger 1))
               (if fit-string
                   (set-fret! pitch-entry fit-string finger)
                   (begin
                     (ly:event-warning
                      (list-ref notes (cdr pitch-entry))
                      (G_ "No string for pitch ~a (given frets ~a)")
                      pitch
                      specified-frets)
                     (kill-note! string-fret-fingers
                                 (cdr pitch-entry)))))))
       (sort pitch-alist (lambda (pitch-entry-a pitch-entry-b)
                           (ly:pitch<? (car pitch-entry-b)
                                       (car pitch-entry-a)))))
      string-fret-fingers)) ;; end of determine-frets-and-strings

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

  ;; TODO: Does it make sense to have additional bass strings in a fret-diagram?
  (if (and (not (null? rest))
           (not (null? (ly:context-property context 'additionalBassStrings))))
      (ly:warning (G_ "additional bass strings are not supported by FretBoards")))

  ;; body of determine-frets
  (let* ((predefined-fret-table
          (ly:context-property context 'predefinedDiagramTable))
         (tunings
          (append
           (ly:context-property context 'stringTunings)
           (ly:context-property context 'additionalBassStrings '())))
         (string-count (length tunings))
         (grob (if (null? rest) '() (car rest)))
         (pitches (map (lambda (x) (ly:event-property x 'pitch)) notes))
         (defined-strings (map (lambda (x)
                                 (if (null? x)
                                     x
                                     (or (string-number x) '())))
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
                             (filter (lambda (entry)
                                       (car entry))
                                     string-frets)
                             string-count))))
        (if (null? grob)
            (placement-list->string-frets predefined-fretboard)
            (create-fretboard context grob predefined-fretboard)))))

(define-public
  (determine-frets context notes specified-info . rest)
  (let ((string-fret-finger-list
         (determine-string-fret-finger context notes specified-info rest)))
    (if (list? string-fret-finger-list)
        (ly:context-set-property!
         context 'stringFretFingerList string-fret-finger-list))
    string-fret-finger-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tablature

;; The TabNoteHead tablatureFormat callbacks.

;; Calculate the fret from pitch and string number as letter
;; The fret letter is taken from 'fretLabels if present
(define-public (fret-letter-tablature-format
                context string-number fret-number)
  (let* ((labels (ly:context-property context 'fretLabels))
         (string-tunings (ly:context-property context 'stringTunings))
         (string-count (length string-tunings))
         (letter
          (cond
           ((= 0 (length labels))
            (string (integer->char (+ fret-number (char->integer #\a)))))
           ((and (<= 0 fret-number) (< fret-number (length labels)))
            (list-ref labels fret-number))
           (else
            (ly:warning
             (G_ "No label for fret ~a (on string ~a);
only ~a fret labels provided")
             fret-number string-number (length labels))
            ".")))
         (add-bass-string-nr ;; starting at zero
          (- string-number string-count 1)))
    (make-translate-scaled-markup '(0 . -0.5)
                                  ;; For additional bass strings, we add zero up to three "/"-signs before
                                  ;; the letter, even more bass strings will get numbers, starting with "4".
                                  ;; In the rare case such a string isn't played open, we put out, eg."4b"
                                  (make-concat-markup
                                   (if (> string-number (+ string-count 4))
                                       (list (number->string add-bass-string-nr)
                                             (if (zero? fret-number) "" letter))
                                       (list (make-string (max 0 add-bass-string-nr) #\/)
                                             letter))))))

;; Display the fret number as a number
(define-public (fret-number-tablature-format
                context string-number fret-number)
  (if (integer? fret-number)
      (make-vcenter-markup
       (format #f "~a" fret-number))
      ;; for non-integer fret-number print p.e. "2½"
      (let* ((whole-part (truncate fret-number))
             (remaining (- fret-number whole-part))
             (fret
              (if (and (zero? whole-part) (not (zero? remaining)))
                  ""
                  (format #f "~a" whole-part)))
             (frac
              (if (zero? remaining)
                  ""
                  (format #f "~a" remaining))))
        (make-concat-markup
         (list (make-vcenter-markup fret)
               (make-vcenter-markup
                ;; the value `-2.5' is my choice
                (make-fontsize-markup -2.5 frac)))))))

;; The 5-string banjo has got an extra string, the fifth (duh), which
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
         (string-nr
          (if (> string-number (length string-tunings))
              (1+ (length string-tunings))
              string-number))
         (string-one-topmost (ly:context-property context 'stringOneTopmost))
         (staff-line (- (* 2 string-nr) string-count 1)))
    (if string-one-topmost
        (- staff-line)
        staff-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar numbers

(define-public ((every-nth-bar-number-visible n) barnum mp)
  (= 0 (modulo barnum n)))

(define-public ((modulo-bar-number-visible n m) barnum mp)
  (and (> barnum 1) (= m (modulo barnum n))))

(define-public ((set-bar-number-visibility n) tr)
  (let ((bn (ly:context-property tr 'currentBarNumber)))
    (ly:context-set-property! tr 'barNumberVisibility
                              (modulo-bar-number-visible n (modulo bn n)))))

(define-public (first-bar-number-invisible barnum mp)
  (> barnum 1))

(define-public (first-bar-number-invisible-save-broken-bars barnum mp)
  (or (> barnum 1)
      (> (ly:moment-main-numerator mp) 0)))

(define-public (first-bar-number-invisible-and-no-parenthesized-bar-numbers barnum mp)
  (and (> barnum 1)
       (= (ly:moment-main-numerator mp) 0)))

(define-public (robust-bar-number-function barnum measure-pos alt-number context)
  (define (get-number-and-power an pow)
    (if (<= an alt-number)
        (get-number-and-power (+ an (expt 26 (1+ pow))) (1+ pow))
        (cons (+ alt-number (- (expt 26 pow) an)) (1- pow))))
  (define (make-letter so-far an pow)
    (if (< pow 0)
        so-far
        (let ((pos (modulo (quotient an (expt 26 pow)) 26)))
          (make-letter (string-append so-far
                                      (substring "abcdefghijklmnopqrstuvwxyz"
                                                 pos
                                                 (1+ pos)))
                       an
                       (1- pow)))))
  (let* ((number-and-power (get-number-and-power 0 0))
         (begin-measure (= 0 (ly:moment-main-numerator measure-pos)))
         (maybe-open-parenthesis (if begin-measure "" "("))
         (maybe-close-parenthesis (if begin-measure "" ")")))
    (string-append maybe-open-parenthesis
                   (number->string barnum)
                   (make-letter ""
                                (car number-and-power)
                                (cdr number-and-power))
                   maybe-close-parenthesis)))

(define-public (all-bar-numbers-visible barnum mp) #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; percent repeat counters

(define-public ((every-nth-repeat-count-visible n) count context)
  (= 0 (modulo count n)))

(define-public (all-repeat-counts-visible count context) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pitch recognition

(define-public (make-semitone->pitch pitches)
  "Convert @var{pitches}, an unordered list of note values
covering (after disregarding octaves) all absolute pitches in need of
conversion, into a function converting semitone numbers (absolute
pitch missing enharmonic information) back into note values.

For a key signature without accidentals

@example
c cis d es e f fis g gis a bes b
@end example

might be a good choice, covering Bb@tie{}major to A@tie{}major and their
parallel keys, and melodic/@/harmonic C@tie{}minor to A@tie{}minor."
  ;; TODO: short-circuit lcm calculation once we know it will be large
  (let* ((size (apply lcm (map (lambda (pitch)
                                 (denominator (/ (ly:pitch-tones pitch) 6)))
                               pitches)))
         ;; Normal tunings need 12 steps per octave, quartertone
         ;; tunings 24, Makam needs 108.  But microtunings might cause
         ;; trouble.
         (lookup (if (> size 400)
                     (make-hash-table)
                     (make-vector size #f))))
    (for-each
     (lambda (pitch)
       (let* ((rawoct (/ (ly:pitch-tones pitch) 6))
              (oct (floor rawoct))
              (ref (- rawoct oct))
              (val (ly:pitch-transpose pitch
                                       (ly:make-pitch (- oct) 0))))
         (if (hash-table? lookup)
             (hashv-set! lookup ref val)
             (vector-set! lookup (* size ref) val))))
     pitches)
    (lambda (semitone)
      "Convert @var{semitone} numbers into note values.  If the
originally specified list of pitches does not contain a note
corresponding to @var{semitone} (disregarding octaves), @code{#f} is
returned."
      (let* ((rawoct (/ semitone 12))
             (oct (floor rawoct))
             (ref (- rawoct oct))
             (val (if (hash-table? lookup)
                      (hashv-ref lookup ref)
                      (let ((ref (* (vector-length lookup) ref)))
                        (and (integer? ref)
                             (vector-ref lookup ref))))))
        (and val
             (ly:pitch-transpose val (ly:make-pitch oct 0)))))))

(define-public ((shift-semitone->pitch key semitone->pitch) semitone)
  "Given a function @var{semitone->pitch} converting a semitone number
into a note value for a lookup table created in relation to@tie{}C,
returns a corresponding function in relation to @var{key}.  The note
values returned by this function differ only enharmonically from the
original @var{semitone->pitch} function."
  (ly:pitch-transpose (semitone->pitch (- semitone (* 2 (ly:pitch-tones key))))
                      key))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NoteNames

(define-public (note-name-markup pitch context)
  (let* ((markuplist '())
         (oct-style (ly:context-property context 'printOctaveNames))
         (acc-style (ly:context-property context 'printAccidentalNames))
         (lang (ly:context-property context 'printNotesLanguage))
         (lily-str (symbol->string (note-name->lily-string pitch)))
         (basic-str (if (not (null? lang))
                        (note-name->string pitch (string->symbol lang))
                        (note-name->string pitch))))
    (set! markuplist
          (append markuplist
                  (if acc-style
                      (if (eq? acc-style 'lily)
                          (list lily-str)
                          (list basic-str
                                (accidental->markup (ly:pitch-alteration pitch))))
                      (list basic-str))))
    (if oct-style
        (set! markuplist
              (append markuplist
                      (list
                       (if (eq? oct-style 'scientific)
                           (make-sub-markup
                            (number->string
                             (+ 4 (ly:pitch-octave pitch))))
                           (octave->lily-string pitch))))))
    (make-concat-markup markuplist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markups for OttavaBrackets

(define-public ottavation-numbers
  '((4 . "29")
    (3 . "22")
    (2 . "15")
    (1 . "8")
    (-1 . "8")
    (-2 . "15")
    (-3 . "22")
    (-4 . "29")))

(define-public ottavation-ordinals
  `((4 . ,(make-concat-markup
           (list (make-general-align-markup Y UP "29")
                 (make-general-align-markup Y UP (make-tiny-markup "ma")))))
    (3 . ,(make-concat-markup
           (list (make-general-align-markup Y UP "22")
                 (make-general-align-markup Y UP (make-tiny-markup "ma")))))
    (2 . ,(make-concat-markup
           (list (make-general-align-markup Y UP "15")
                 (make-general-align-markup Y UP (make-tiny-markup "ma")))))
    (1 . ,(make-concat-markup
           (list (make-general-align-markup Y UP "8")
                 (make-general-align-markup Y UP (make-tiny-markup "va")))))
    (-1 . ,(make-concat-markup
            (list "8" (make-tiny-markup "va"))))
    (-2 . ,(make-concat-markup
            (list "15" (make-tiny-markup "ma"))))
    (-3 . ,(make-concat-markup
            (list "22" (make-tiny-markup "ma"))))
    (-4 . ,(make-concat-markup
            (list "29" (make-tiny-markup "ma"))))))

;; former default
(define-public ottavation-simple-ordinals
  '((4 . "29ma")
    (3 . "22ma")
    (2 . "15ma")
    (1 . "8va")
    (-1 . "8vb")
    (-2 . "15mb")
    (-3 . "22mb")
    (-4 . "29mb")))
