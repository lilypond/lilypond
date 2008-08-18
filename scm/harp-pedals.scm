;;;; harp-pedals.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2008 Reinhold Kainhofer <reinhold@kainhofer.com>


;;;; More verbose version, which takes a list of directions. It's commented
;;;; out, because it has some issues (see below) and does not add any new
;;;; functionality over \harp-pedal
;; (define-builtin-markup-command (harp-pedal-verbose layout props pedal-list) (list?)
;;   music ; markup type
;;   ((size 1.0)
;;    (harp-pedal-details)
;;    (thickness 0.5))
;;   "Make a harp pedal diagram containing the directions indicated in @var{pedal-list}.
;;
;;   For example,
;;
;; @example
;; \\markup \\pedal-diagram-verbose #'(1 0 -1 #\\| 0 0 1 1)
;; \\markup \\pedal-diagram-verbose #(list UP CENTER DOWN #\\| CENTER CENTER UP UP)
;; @end example
;; "
;;   (make-harp-pedal layout props pedal-list))


(define-builtin-markup-command (harp-pedal layout props definition-string) (string?)
  music ; markup type for the documentation!
  ((size 1.0)
   (harp-pedal-details)
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
pedals, then the divider and then the remaining four pedals. If not it
prints out a warning. However, in any case, it will also print each symbol
in the order as given. This means you can place the divider (even multiple
dividers) anywhere you want, but you'll have to live with the warnings.

The appearance of the diagram can be tweaked inter alia using the size property
of the TextScript grob (@code{\\override Voice.TextScript #'size = #0.3}) for
the overall, the thickness property
(@code{\\override Voice.TextScript #'thickness = #3}) for the line thickness of
the horizontal line and the divider. The remaining configuration (box sizes,
offsets and spaces) is done by the harp-pedal-details  list of properties
(@code{\\override Voice.TextScript #'harp-pedal-details #'box-width = #1}).
It contains the following settings: @code{box-offset} (vertical shift of the
box center for up/down pedals), @code{box-width}, @code{box-height},
@code{space-before-divider} (the spacing between two boxes before the
divider) and @code{space-after-divider} (box spacing after the divider).

@lilypond[verbatim,quote]
\\markup \\harp-pedal #\"^-v|--ov^\"
@end lilypond
"

;; There is also a \harp-pedal-verbose version, which
;; takes a list of -1/0/1 directions and a possible |. Unfortunately, it has some
;; caveats:
;;   1) the | cannot be given as a string "|" but as a character #\| and
;;      the "o" has to be given as #\o.
;;   2) if one wants to use directions like UP, CENTER or DOWN, one cannot use
;;      '(UP DOWN CENTER #\| ....), because the contents of that list are
;;      never evaluated to -1/0/1. Instead one has to explicitly create a
;;      list like (list UP DOWN CENTER #\| ....)

  (make-harp-pedal layout props (harp-pedals-parse-string definition-string)))


(define (harp-pedals-parse-string definition-string)
 "Parse a harp pedals diagram string and return a list containing 1, 0, -1, #\\o or #\\|"
  (map (lambda (c)
    (case c
      ((#\^) 1)
      ((#\v) -1)
      ((#\-) 0)
      ((#\| #\o) c)
      (else c)))
    (string->list definition-string)))

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

(define (harp-pedal-check pedal-list)
  "Perform some sanity checks for harp pedals (7 pedals, divider after third)"
  (let ((info (harp-pedal-info pedal-list)))
    ; 7 pedals:
    (if (not (equal? (car info) 7))
      (ly:warning "Harp pedal diagram contains ~a pedals rather than the usual 7." (car info)))
    ; One divider after third pedal:
    (if (null? (cdr info))
      (ly:warning "Harp pedal diagram does not contain a divider (usually after third pedal).")
      (if (not (equal? (cdr info) '(3)))
        (ly:warning "Harp pedal diagram contains dividers at positions ~a. Normally, there is only one divider after the third pedal." (cdr info))))))


(define (make-harp-pedal layout props pedal-list)
  "Make a harp pedals diagram markup"


  ; FIXME the size variable should be defined by a prop. lookup
  (harp-pedal-check pedal-list)

  (let* ((size (chain-assoc-get 'size props 1.2))
        (details (chain-assoc-get 'harp-pedal-details props '()))
        (dy (* size (assoc-get 'box-offset details 0.8))) ; offset of the box center from the line
        (line-width (* (ly:output-def-lookup layout 'line-thickness)
                       (chain-assoc-get 'thickness props 0.5)))
        (box-width (* size (assoc-get 'box-width details 0.4)))
        (box-hheight (* size (/ (assoc-get 'box-height details 1.0) 2))) ; half the box-height, saves some divisions by 2
        (spacebeforedivider (* size (assoc-get 'space-before-divider details 0.8))) ; full space between boxes before the first divider
        (spaceafterdivider (* size (assoc-get 'space-after-divider details 0.8))) ; full space between boxes
        ;(spacebeforedivider (/ (+ box-width (* 8 spaceafterdivider)) 8))
        (box-x-dimensions (lambda (prev-x p space) (cons (+ prev-x space)
                                                   (+ prev-x space box-width))))
        (box-y-dimensions (lambda (prev-x p space) (cons (- (* p dy) box-hheight)
                                                         (+ (* p dy) box-hheight))))
        (divider-stencil (lambda (xpos) (make-line-stencil line-width xpos (- 0 dy box-hheight) xpos (+ dy box-hheight))))
        (result (let process-pedal  ((remaining pedal-list)
                                     (prev-x 0)
                                     (stencils '())
                                     (circled #f)
                                     (space spacebeforedivider))
          ; Terminal condition of the recursion, return (final-x . stencil-list)
          (if (null? remaining)
            (cons (+ prev-x space) stencils)

            (case (car remaining)
              ((1 0 -1)  ; Pedal up/neutral/down
                  (let* ((p (car remaining))
                        (stencil (make-filled-box-stencil
                                   (box-x-dimensions prev-x p space)
                                   (box-y-dimensions prev-x p space)))
                                   ;(circle-stencil (if circled (rounded-box-stencil stencil 0.05 0.3 0.1 ) stencil))
                                   (circle-stencil (if circled (circle-stencil stencil 0.05 0.2 ) stencil))
                        (new-prev-x (+ prev-x space box-width)))
                      (process-pedal (cdr remaining) new-prev-x (cons circle-stencil stencils) #f space)))
              ((#\|)  ; Divider line
                  (let* ((xpos (+ prev-x space))
                         (stencil (divider-stencil xpos))
                         (new-prev-x (+ prev-x space)))
                    (process-pedal (cdr remaining) new-prev-x (cons stencil stencils) circled spaceafterdivider)))
              ((#\o)  ; Next pedal should be circled
                  (process-pedal (cdr remaining) prev-x stencils #t space))
              (else
                  (ly:warning "Unhandled entry in harp-pedal: ~a" (car remaining))
                  (process-pedal (cdr remaining) prev-x stencils circled space))))))
        (final-x (car result))
        (stencils (reverse (cdr result))))
    ; Add the horizontal line and combine all stencils:
    (apply ly:stencil-add
      (cons
        (make-line-stencil line-width 0 0 final-x 0)
        stencils))))

