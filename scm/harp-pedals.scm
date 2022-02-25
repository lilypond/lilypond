;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2008--2022 Reinhold Kainhofer <reinhold@kainhofer.com>
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



(define-markup-command (harp-pedal layout props definition-string) (string?)
  #:category instrument-specific-markup ; markup type for the documentation!
  #:properties ((size 1.2)
                (harp-pedal-details '())
                (thickness 0.5))
  "Make a harp pedal diagram.

Possible elements in @var{definition-string}:

@table @code
@item ^
pedal is up
@item -
pedal is neutral
@item v
pedal is down
@item |
vertical divider line
@item o
the following pedal should be circled (indicating a change)
@end table

The function also checks if the string has the typical form of three
pedals, then the divider and then the remaining four pedals.  If not it
prints out a warning.  However, in any case, it will also print each symbol
in the order as given.  This means you can place the divider (even
multiple dividers) anywhere you want, but you'll have to live with the
warnings.

The appearance of the diagram can be tweaked inter alia using the size property
of the TextScript grob (@code{\\override Voice.TextScript.size = #0.3}) for
the overall, the thickness property (@code{\\override
Voice.TextScript.thickness = #3}) for the line thickness of
the horizontal line and the divider.  The remaining configuration (box
sizes, offsets and spaces) is done by the harp-pedal-details list of
properties (@code{\\override Voice.TextScript.harp-pedal-details.box-width = #1}).
It contains the following settings: @code{box-offset} (vertical shift
of the box center for up/down pedals), @code{box-width},
@code{box-height}, @code{space-before-divider} (the spacing between
two boxes before the divider) and @code{space-after-divider} (box
spacing after the divider).

@lilypond[verbatim,quote]
\\markup \\harp-pedal #\"^-v|--ov^\"
@end lilypond
"
  (let* ((pedal-list (harp-pedals-parse-string definition-string))
         (details (begin (harp-pedal-check pedal-list) harp-pedal-details))
         (dy (* size (assoc-get 'box-offset details 0.8))) ; offset of the box center from the line
         (line-width (* (ly:output-def-lookup layout 'line-thickness)
                        thickness))
         (box-width (* size (assoc-get 'box-width details 0.4)))
         (box-hheight (* size (/ (assoc-get 'box-height details 1.0) 2))) ; half the box-height, saves some divisions by 2
         (spacebeforedivider (* size (assoc-get 'space-before-divider details 0.8))) ; full space between boxes before the first divider
         (spaceafterdivider (* size (assoc-get 'space-after-divider details 0.8))) ; full space between boxes
         (circle-thickness (* (ly:output-def-lookup layout 'line-thickness)
                              (assoc-get 'circle-thickness details 0.5)))
         (circle-x-padding (* size (assoc-get 'circle-x-padding details 0.15)))
         (circle-y-padding (* size (assoc-get 'circle-y-padding details 0.2)))
         (box-x-dimensions (lambda (prev-x p space) (cons (+ prev-x space)
                                                          (+ prev-x space box-width))))
         (box-y-dimensions (lambda (prev-x p space) (cons (- (* p dy) box-hheight)
                                                          (+ (* p dy) box-hheight))))
         (divider-stencil (lambda (xpos) (make-line-stencil line-width
                                                            xpos (- 0 dy box-hheight)
                                                            xpos (+ dy box-hheight))))
         (result (let process-pedal  ((remaining pedal-list)
                                      (prev-x 0)
                                      (stencils '())
                                      (circled #f)
                                      (space spacebeforedivider))
                   ;; Terminal condition of the recursion, return (final-x . stencil-list)
                   (if (null? remaining)
                       (cons (+ prev-x space) (reverse stencils))

                       (case (car remaining)
                         ((1 0 -1)  ; Pedal up/neutral/down
                          (let* ((p (car remaining))
                                 (stencil (make-filled-box-stencil
                                           (box-x-dimensions prev-x p space)
                                           (box-y-dimensions prev-x p space)))
                                 (pedal-stencil
                                  (if circled
                                      (oval-stencil stencil circle-thickness
                                                    circle-x-padding circle-y-padding)
                                      stencil))
                                 (new-prev-x (+ prev-x space box-width)))
                            (process-pedal (cdr remaining) new-prev-x
                                           (cons pedal-stencil stencils) #f space)))
                         ((#\|)  ; Divider line
                          (let* ((xpos (+ prev-x space))
                                 (stencil (divider-stencil xpos))
                                 (new-prev-x (+ prev-x space)))
                            (process-pedal (cdr remaining) new-prev-x
                                           (cons stencil stencils)
                                           circled spaceafterdivider)))
                         ((#\o)  ; Next pedal should be circled
                          (process-pedal (cdr remaining) prev-x stencils #t space))
                         (else
                          (ly:warning (G_ "Unhandled entry in harp-pedal: ~a")
                                      (car remaining))
                          (process-pedal (cdr remaining)
                                         prev-x stencils circled space))))))
         (final-x (car result))
         (stencils (cdr result)))
    ;; Add the horizontal line and combine all stencils:
    (apply ly:stencil-add
           (make-line-stencil line-width 0 0 final-x 0) ; the horizontal line
           (make-transparent-box-stencil ; space for absent boxes
            (cons 0 final-x)
            (interval-widen '(0 . 0) (+ box-hheight dy)))
           stencils)))

;; Parse the harp pedal definition string into list of directions (-1/0/1), #\o and #\|
;; Whitespace is removed from definition string before the procedure applies.
(define (harp-pedals-parse-string definition-string)
  "Parse a harp pedals diagram string and return a list containing 1, 0, -1, #\\o or #\\|"
  (map (lambda (c)
         (case c
           ((#\^) 1)
           ((#\v) -1)
           ((#\-) 0)
           ((#\| #\o) c)
           (else c)))
       (string->list (remove-whitespace definition-string))))


;; Analyze the pedal-list: Return (pedalcount . (divider positions))
(define (harp-pedal-info pedal-list)
  (let check ((pedals pedal-list)
              (pedalcount 0)
              (dividerpositions '()))
    (if (null? pedals)
        (cons pedalcount (reverse dividerpositions))

        (case (car pedals)
          ((-1 0 1) (check (cdr pedals) (+ pedalcount 1) dividerpositions))
          ((#\|)    (check (cdr pedals) pedalcount (cons pedalcount dividerpositions)))
          (else     (check (cdr pedals) pedalcount dividerpositions))))))


;; Sanity checks, spit out warning if pedal-list violates the conventions
(define (harp-pedal-check pedal-list)
  "Perform some sanity checks for harp pedals (7 pedals, divider after third)"
  (let ((info (harp-pedal-info pedal-list)))
    ;; 7 pedals:
    (if (not (equal? (car info) 7))
        (ly:warning (G_ "Harp pedal diagram contains ~a pedals rather than the usual 7.") (car info)))
    ;; One divider after third pedal:
    (if (null? (cdr info))
        (ly:warning (G_ "Harp pedal diagram does not contain a divider (usually after third pedal)."))
        (if (not (equal? (cdr info) '(3)))
            (ly:warning (G_ "Harp pedal diagram contains dividers at positions ~a.  Normally, there is only one divider after the third pedal.") (cdr info))))))
