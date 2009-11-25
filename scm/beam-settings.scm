;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2009 Carl Sorensen <c_sorensen@byu.edu>
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

;;; specify default beaming rules

;;; format:
;;;
;;; alist of
;;;   ((time-signature function) . (beam-type . grouping)) entries
;;;
;;; where
;;;
;;;     time-signature = time signature fraction (pair of numbers, (4 . 4) )
;;;     function = 'end or 'subdivide (possibly implement 'begin in the future)
;;;     beam-type = '* or (numerator . denominator); e.g. (1 . 16)
;;;       NOTE: numerator is kept in beam-type because of
;;;             tuplets, e.g. (2 . 24) = (2 . 3) * (1 . 8)
;;;             for eighth-note triplets.
;;;     grouping = a list of groups, in units of time signature denominator
;;;         (for default rules) or beam-type (for explicit rules)
;;;

(define-public default-beam-settings
 `(
   ;; in 2 2 time:
   ;;   default: end beams on 1 2 note boundaries
   ;;   end beams with 32nd notes each 1 4 beat
   (((2 . 2) end) .
    ((* . (1 1))
    ((1 . 32) . (8 8 8 8))))

   ;; in 2 4, 2 8 and 2 16 time:
   ;;   default: end beams on beats
   (((2 . 4) end) . ((* . (1 1))))
   (((2 . 8) end) . ((* . (1 1))))
   (((2 . 16) end) . ((* . (1 1))))

   ;; in 3 2 time:
   ;;   default: end beams on beats
   ;;   end beams with 32nd notes each 1 4 beat
   (((3 . 2) end) .
    ((* . (1 1 1))
     ((1 . 32) . (8 8 8 8 8 8))))

    ;; in 3 4 time:
    ;;   default: set grouping to (3) so we'll get
    ;;     the proper measure grouping symbol
    ;;
    ;;   set all beams to end on beats, but 1 8 to beam entire measure
   (((3 . 4) end) .((* . (3))
                    ((1 . 8) . (6))
                    ((1 . 16) . (4 4 4))
                    ((1 . 32) . (8 8 8))
                    ((1 . 64) . (16 16 16))
                    ((1 . 128) . (32 32 32))))

    ;; in 3 8 and 3 16 time time:
    ;;   default: group on 3
   (((3 . 8) end) . ((* . (3))))
   (((3 . 16) end) . ((* . (3))))

    ;; in 4 2 time:
    ;;   default: end beams on beats
    ;;   end beams with 16th notes each 1 4 beat
    ;;   end beams with 32nd notes each 1 8 beat
   (((4 . 2) end) .
    ((* . (1 1 1 1))
     ((1 . 16) . (4 4 4 4 4 4 4 4))
     ((1 . 32) . (4 4 4 4
                  4 4 4 4
                  4 4 4 4
                  4 4 4 4))))

    ;; in 4 4 (common) time:
    ;;   default: end beams on beats
    ;;   end beams with 8th notes each 1 2 beat
   (((4 . 4) end) .
    ((* . (1 1 1 1))
     ((1 . 8) . (4 4))))

    ;; in 4 8 time:
    ;;   default: group on 1 4 notes
   (((4 . 8) end) . ((* . (2 2))))

    ;; in 4 16 time:
    ;;   default: group on beats
   (((4 . 16) end) . ((* . (1 1 1 1))))

    ;; in 6 4 time:
    ;;   default group at 3 4
    ;;   end beams with 16th or 32nd notes each 1 4 beat
   (((6 . 4) end) .
    ((* . (3 3))
     ((1 . 16) . (4 4 4 4 4 4))
     ((1 . 32) . (8 8 8 8 8 8))))

    ;; in 6 8 time:
    ;;   default: group at 3 8
    ;;   end beams with 32nd notes each 1 8 beat
   (((6 . 8) end) .
    ((* . (3 3))
     ((1 . 32) . (4 4 4 4 4 4))))

    ;; in 6 16 time:
    ;;   default: group at 3 16
   (((6 . 16) end) . ((* . (3 3))))

    ;; in 9 4 time:
    ;;   default: group at 3 4
    ;;   end beams with 16th or 32nd notes each 1 4 beat
   (((9 . 4) end) .
    ((* . (3 3 3))
     ((1 . 16) . (4 4 4 4 4 4 4 4 4))
     ((1 . 32) . (8 8 8 8 8 8 8 8 8))))

    ;; in 9 8 time:
    ;;   default: group at 3 8
    ;;   use beatGrouping for all except 32nd notes
    ;;   end beams with 32nd notes each 1 8 beat
   (((9 . 8) end) .
    ((* . (3 3 3))
     ((1 . 32) . (4 4 4 4 4 4 4 4 4))))

    ;; in 9 16 time
    ;;   default: group at 3 8
   (((9 . 16) end) . ((* . (3 3 3))))

    ;; in 12 4 time:
    ;;   default: group at 3 4
    ;;   end beams with 16th or 32nd notes each 1 4 beat
   (((12 . 4) end) .
    ((* . (3 3 3 3))
     ((1 . 16) . (4 4 4 4 4 4 4 4 4 4 4 4 4))
     ((12 . 4) . (8 8 8 8 8 8 8 8 8 8 8 8 8))))

    ;; in 12 8 time:
    ;;   default: group at 3 8
    ;;   end beams with 32nd notes each 1 8 beat
   (((12 . 8) end) .
    ((* . (3 3 3 3))
     ((1 . 32) . (4 4 4 4 4 4 4 4 4 4 4 4 4))))

    ;; in 12 16 time:
    ;;   default: group at 3 16
   (((12 . 16) end) . ((* . (3 3 3 3))))

    ;; in 5 8 time:
    ;;   default: group (3 2)
   (((5 . 8) end) . ((* . (3 2))))

    ;; in 8 8 time:
    ;;   default: group (3 3 2)
   (((8 . 8) end) . ((* . (3 3 2))))
  ))  ; end of alist definition

;;; Functions for overriding beam settings
;;;

(define (overridden-property-alist context property setting value)
  "Return an alist containing the current @{context} value of
@code{property} overriden by @code{(setting . value)}. "
  (cons (cons setting value) (ly:context-property context property)))

(define-public (override-property-setting context property setting value)
  "Like the C++ code that executes \\override, but without type
checking. "
  (ly:context-set-property!
    context property
    (overridden-property-alist context property setting value)))

(define (revert-property-setting context property setting)
  "Like the C++ code that executes \revert, but without type
checking. "

  (define (revert-member alist entry new)
    "Return ALIST, with ENTRY removed.  ALIST is not modified, instead
a fresh copy of the list-head is made."
    (cond
      ((null? alist) new)
      ((equal? (car alist) entry) (revert-member (cdr alist) entry new))
      (else (revert-member (cdr alist) entry (cons (car alist) new)))))

  (ly:context-set-property!
    context property
    (revert-member (ly:context-property context property) setting '())))

(define-public (override-beam-setting
                  time-signature rule-type rule . rest)
  "Override the beam settings for the context in @var{rest},
for @var{time-signature} and  @var{rule-type}, with the
new rule alist @var{rule}. "
  (define (make-setting c)
    (let ((new-settings
            (overridden-property-alist
              c
              'beamSettings
              (list time-signature rule-type)
              rule)))
      (ly:context-set-property! c 'beamSettings new-settings)))

  (let ((music-to-export
          (context-spec-music
            (make-apply-context make-setting)
              (if (and (pair? rest) (symbol? (car rest)))
                  (car rest)
                  'Voice))))
    (ly:export music-to-export)))

(define-public (score-override-beam-setting
                 time-signature rule-type rule)
  (override-beam-setting
    time-signature rule-type rule 'Score))

(define-public (revert-beam-setting
                  time-signature rule-type . rest)
  (ly:export
    (context-spec-music
      (make-apply-context
        (lambda (c)
          (revert-property-setting
            c
            'beamSettings
            (list time-signature rule-type))))
      (if (and (pair? rest) (symbol? (car rest)))
          (car rest)
          'Voice))))
