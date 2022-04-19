;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2022 Carl D. Sorensen <c_sorensen@byu.edu>
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

;;  Utility functions

(define (cons-fret new-value old-list)
  "Put together a fret-list in the format desired by parse-string (to work on a
fret-diagram-definition-string)."
  (if (pair? old-list)
      (cons* new-value old-list)
      (list new-value)))

(define (get-numeric-from-key keystring)
  "Get the numeric value from a key of the form k:val of a
fret-diagram-definition-string."
  (let* ((entries (string-split keystring #\:)))
    ;; print a warning if more than one ":"-sign was entered by accident
    (if (> (length (cdr entries)) 1)
        (ly:warning (G_ "Possible typo in fret-diagram '~a'") keystring))
    ;; throw an error, if `entry' can't be transformed into a number
    (or (string->number (last entries))
        (ly:error
         "Unhandled entry in fret-diagram \"~a\" in \"~a\""
         (last entries)
         keystring))))

(define (numerify mylist)
  "Convert string values of @var{mylist} to numeric or character (to work on a
list derived from a fret-diagram-definition-string)."
  (map
   (lambda (entry)
     (or (string->number entry)
         ;; TODO will entry ever be not a number-string?
         ;;      for now we let in in
         (car (string->list entry))))
   ;; clear for empty-strings, may happen with forgotten closing barre-paren
   (remove string-null? mylist)))

(define (fret-count fret-range)
  "Calculate the fret count for the diagram given the range of frets."
  (1+ (- (cdr fret-range) (car fret-range))))

(define (dot-has-color dot-settings)
  "Return a color-name as symbol, if found in @var{dot-settings}
otherwise @code{#f}.  Campared with color-names from @code{x11-color-list}."
  ;; `x11-color-list' is not available for a toplevel definition here, thus avoid
  ;; to call it again and again by a recursion use simple filter instead.
  (let* ((colors
          (filter
           (lambda (dot-setting)
             ;; if dot-setting is not a symbol get #f pretty early
             (and (symbol? dot-setting)
                  (assoc-get dot-setting x11-color-list)))
           dot-settings)))
    (if (pair? colors) (car colors) #f)))

(define (dot-is-inverted dot-settings)
  "Return @code{'inverted}, if found in @var{dot-settings} otherwise @code{#f}"
  (if (member 'inverted dot-settings) 'inverted #f))

(define (dot-is-parenthesized dot-settings)
  "Return @code{'parenthesized}, if found in @var{dot-settings}
otherwise @code{#f}"
  (if (member 'parenthesized dot-settings) 'parenthesized #f))

;; If @code{'default-paren-color} is not set, the parenthesis will take their
;; color from the dot.
;; Setting @code{'default-paren-color} will result in taking the color from
;; `what-color', see below.
(define (default-paren-color dot-settings)
  "Return @code{'default-paren-color}, if found in @var{dot-settings}
otherwise @code{#f}"
  (if (member 'default-paren-color dot-settings) 'default-paren-color #f))

(define (subtract-base-fret base-fret dot-list)
  "Subtract @var{base-fret} from every fret in @var{dot-list}"
  (map
   (lambda (this-list)
     (list
      ;; string
      (car this-list)
      ;; fret
      (- (second this-list) base-fret)
      ;; finger-number or markup
      (if (and (not (null? (cddr this-list)))
               (or (markup? (caddr this-list))
                   (number? (caddr this-list))))
          (third this-list)
          '())
      ;; inverted
      (or (dot-is-inverted this-list) '())
      ;; parenthesis
      (or (dot-is-parenthesized this-list) '())
      ;; color modifiers
      ;; parenthesis
      (or (default-paren-color this-list) '())
      ;; dots
      (let ((colored (dot-has-color this-list)))
        (if colored
            colored
            '()))))
   dot-list))

(define (get-sub-list value master-list)
  "Get a sub-list from @var{master-list} whose last element is equal to
@var{value} or @code{#f}.  Take the first matching sub-list."
  ;; Used to keep track of barre-indicators in a fret-diagram-definition-string.
  (cond ((null? master-list) #f)
        ((eqv? (last (car master-list)) value) (car master-list))
        (else (get-sub-list value (cdr master-list)))))

(define (merge-details key alist-list . default)
  "Return @code{alist-list} entries for @code{key}, in one combined alist.
There can be two @code{alist-list} entries for a given key.  The first comes
from the override-markup function, the second comes from property settings
during a regular override.
This is necessary because some details can be set in one place, while others are
set in the other.  Both details lists must be merged into a single alist.
Return @code{default} (optional, else #f) if not found."

  (define (helper key alist-list default)
    (if (null? alist-list)
        default
        (let* ((entry (assoc-get key (car alist-list))))
          (if entry
              (append entry (chain-assoc-get key (cdr alist-list) '()))
              (helper key (cdr alist-list) default)))))

  (helper key alist-list (if (pair? default) (car default) #f)))

;;  Conversions between fret/string coordinate system and x-y coordinate
;;  system.
;;
;;  Fret coordinates are measured down the fretboard from the nut,
;;   starting at 0.
;;
;; String coordinates are measured from the lowest string, starting at 0.
;;
;; The x-y origin is at the intersection of the nut and the lowest string.
;;
;; X coordinates are positive to the right.
;; Y coordinates are positive up.

(define (negate-extent extent)
  "Return the extent in an axis opposite to the axis of @code{extent}."
  (cons (- (cdr extent)) (- (car extent))))

(define (stencil-fretboard-extent stencil fretboard-axis orientation)
  "Return the extent of @code{stencil} in the @code{fretboard-axis}
direction."
  (if (eq? fretboard-axis 'fret)
      (cond ((eq? orientation 'landscape)
             (ly:stencil-extent stencil X))
            ((eq? orientation 'opposing-landscape)
             (negate-extent (ly:stencil-extent stencil X)))
            (else
             (negate-extent (ly:stencil-extent stencil Y))))
      ;; else -- eq? fretboard-axis 'string
      ;; only 'opposing-landscape needs to be special-cased:
      (cond ((eq? orientation 'opposing-landscape)
             (negate-extent (ly:stencil-extent stencil Y)))
            (else
             (ly:stencil-extent stencil Y)))))

(define (stencil-fretboard-offset stencil fretboard-axis orientation)
  "Return the stencil coordinates of the center of @var{stencil}
in the @var{fretboard-axis} direction.  @var{fretboard-axis} may take
@code{'fret} or @code{'string}.  @var{orientation} is either @code{'normal},
(the default), @code{'landscape} or @code{opposing-landscape}."
  ;; `fretboard-axis' and `orientation' is needed to let
  ;; `stencil-fretboard-extent' from above select property.
  (* 0.5 (interval-length
          (stencil-fretboard-extent stencil fretboard-axis orientation))))

(define (string-thickness string thickness-factor)
  (expt (1+ thickness-factor) (1- string)))

;;  Functions that create stencils used in the fret diagram

(define (sans-serif-stencil layout props mag text)
  "Create a stencil in sans-serif font based on @var{layout} and @var{props}
with magnification @var{mag} of the string @var{text}."
  (let* ((my-props
          (prepend-alist-chain
           'font-size (magnification->font-size mag)
           (prepend-alist-chain 'font-family 'sans props))))
    (interpret-markup layout my-props text)))

;;  markup commands and associated functions

(define (fret-parse-marking-list marking-list my-fret-count)
  "Parse a fret-diagram-verbose marking list into component sublists"
  (let* ((fret-range (cons 1 my-fret-count))
         (capo-fret 0)
         (barre-list '())
         (dot-list '())
         (xo-list '())
         (output-alist '()))
    (let parse-item ((mylist marking-list))
      (if (not (null? mylist))
          (let* ((my-item (car mylist)) (my-code (car my-item)))
            (cond
             ((or (eq? my-code 'open)(eq? my-code 'mute))
              (set! xo-list (cons* my-item xo-list)))
             ((eq? my-code 'barre)
              (if (every number? (cdr my-item))
                  (set! barre-list (cons* (cdr my-item) barre-list))
                  (ly:error
                   "barre-indications should contain only numbers: ~a"
                   (cdr my-item))))
             ((eq? my-code 'capo)
              (set! capo-fret (cadr my-item)))
             ((eq? my-code 'place-fret)
              (set! dot-list (cons* (cdr my-item) dot-list)))
             (else (ly:warning
                    "unknown fret diagram placement code: ~a"
                    (object->string my-code))))
            (parse-item (cdr mylist)))))
    ;; calculate fret-range
    (let ((maxfret 0)
          (minfret (if (> capo-fret 0) capo-fret 99)))
      (let updatemax ((fret-list dot-list))  ;CHANGE THIS TO HELPER FUNCTION?
        (if (null? fret-list)
            '()
            (let ((fretval (second (car fret-list))))
              (if (> fretval maxfret) (set! maxfret fretval))
              (if (< fretval minfret) (set! minfret fretval))
              (updatemax (cdr fret-list)))))
      ;; take frets of 'barre-settings into account
      (if (not (null? barre-list))
          (set! minfret (apply min minfret (map last barre-list))))
      (if (or (> maxfret my-fret-count) (> capo-fret 1))
          (set! fret-range
                (cons minfret
                      (let ((upfret (1- (+ minfret my-fret-count))))
                        (if (> maxfret upfret) maxfret upfret)))))
      (if (not (zero? (apply min capo-fret (map cadr dot-list))))
          (set! capo-fret (1+ (- capo-fret minfret))))
      ;; subtract fret from dots
      (set! dot-list (subtract-base-fret (1- (car fret-range)) dot-list)))
    (acons 'fret-range fret-range
           (acons 'barre-list barre-list
                  (acons 'dot-list dot-list
                         (acons 'xo-list xo-list
                                (acons 'capo-fret capo-fret '())))))))

(define (make-fret-diagram layout props marking-list)
  "Make a fret diagram markup"
  (let* (
         ;; note: here we get items from props that are needed in this routine,
         ;; or that are needed in more than one of the procedures
         ;; called from this routine.  If they're only used in one of the
         ;; sub-procedure, they're obtained in that procedure
         (size (chain-assoc-get 'size props 1.0)) ; needed for everything
         ;;TODO -- get string-count directly from length of stringTunings;
         ;;         from FretBoard engraver, but not from markup call
         (details (merge-details 'fret-diagram-details props '()))
         (fret-distance
          (assoc-get 'fret-distance details 1.0))
         (string-distance-from-details
          (assoc-get 'string-distance details 1.0))
         ;; disable negative `string-distance'
         ;; mmh -- should we print a message/warning?
         (string-distance (abs string-distance-from-details))
         (handedness (assoc-get 'handedness details RIGHT))
         (string-count
          (assoc-get 'string-count details 6)) ;; needed for everything
         (my-fret-count
          (assoc-get 'fret-count details 4)) ;; needed for everything
         (orientation
          (assoc-get 'orientation details 'normal)) ;; needed for everything
         (finger-code
          (assoc-get
           'finger-code details 'none)) ;; needed for draw-dots and draw-barre
         (default-dot-radius
           (if (eq? finger-code 'in-dot) 0.425 0.25)) ;; bigger dots if labeled
         (dot-radius
          (assoc-get 'dot-radius details default-dot-radius))
         (default-dot-position
           (if (eq? finger-code 'in-dot)
               (- 0.95 default-dot-radius)
               0.6)) ; move up to make room for bigger dot if labeled
         ;; needed for draw-dots and draw-barre
         (dot-position
          (assoc-get 'dot-position details default-dot-position))
         ;; default thickness
         ;; needed for draw-dots and draw-barre
         (th
          (* (ly:output-def-lookup layout 'line-thickness)
             (chain-assoc-get 'thickness props 0.5)))
         ;; needed for draw-frets and draw-strings
         (sth (* size th))
         (thickness-factor (assoc-get 'string-thickness-factor details 0))
         (paren-padding (assoc-get 'paren-padding details 0.05))
         (alignment (chain-assoc-get 'align-dir props -0.4)) ;; needed only here
         (xo-padding (assoc-get 'xo-padding details 0.2)) ;; needed only here
         (parameters (fret-parse-marking-list marking-list my-fret-count))
         (capo-fret (assoc-get 'capo-fret parameters 0))
         (dot-list (assoc-get 'dot-list parameters))
         (xo-list (assoc-get 'xo-list parameters))
         (fret-range (assoc-get 'fret-range parameters))
         (my-fret-count (fret-count fret-range))
         (barre-list (assoc-get 'barre-list parameters))
         (barre-type (assoc-get 'barre-type details 'curved))
         (fret-diagram-stencil '()))

    ;;  Here are the fret diagram helper functions that depend on the
    ;;  fret diagram parameters.  The functions are here because the
    ;;  diagram parameters are part of the lexical scope here.

    (define (stencil-coordinates fret-coordinate string-coordinate)
      "Return a pair @code{(x-coordinate . y-coordinate)}
      in stencil coordinate system."
      (cond
       ((eq? orientation 'landscape)
        (cons fret-coordinate
              (* handedness (- string-coordinate (1- string-count)))))
       ((eq? orientation 'opposing-landscape)
        (cons (- fret-coordinate) (* handedness (- string-coordinate))))
       (else
        (cons (* handedness string-coordinate) (- fret-coordinate)))))

    (define (stencil-coordinate-offset fret-offset string-offset)
      "Return a pair @code{(x-offset . y-offset)}
      for translation in stencil coordinate system."
      (cond
       ((eq? orientation 'landscape)
        (cons fret-offset (- string-offset)))
       ((eq? orientation 'opposing-landscape)
        (cons (- fret-offset) string-offset))
       (else
        (cons string-offset (- fret-offset)))))

    (define (make-bezier-sandwich-list start stop base height half-thickness)
      "Make the argument list for a bezier sandwich from
string coordinate @var{start} to string-coordinate @var{stop} with a
baseline at fret coordinate @var{base}, a height of
@var{height}, and a thickness of @var{half-thickness}."
      (let* ((width (1+ (- stop start)))
             (cp-left-width (+ (* width half-thickness) start))
             (cp-right-width (- stop (* width half-thickness)))
             (bottom-control-point-height
              (- base (- height half-thickness)))
             (top-control-point-height
              (- base height))
             (left-start-end-point
              (stencil-coordinates base start))
             (right-end-point
              (stencil-coordinates base stop))
             (left-upper-control-point
              (stencil-coordinates
               top-control-point-height cp-left-width))
             (left-lower-control-point
              (stencil-coordinates
               bottom-control-point-height cp-left-width))
             (right-upper-control-point
              (stencil-coordinates
               top-control-point-height cp-right-width))
             (right-lower-control-point
              (stencil-coordinates
               bottom-control-point-height cp-right-width)))
        ;; order of bezier control points is:
        ;;   left cp start/end, left cp low, right cp low, right cp end,
        ;;   right cp high, left cp high
        ;;
        ;;          left-upper ← ← ← ← ← ← ← right-upper
        ;;           ↙    left-lower → → right-lower   ↖
        ;;         ↙       ↗                    ↘        ↖
        ;;     left-start-end                    right-end
        (list
         left-start-end-point
         left-lower-control-point
         right-lower-control-point
         right-end-point

         right-upper-control-point
         left-upper-control-point)))

    (define (draw-strings)
      "Draw the string lines for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}.  Orientation is determined by @var{orientation}."
      (let* ((string-list (iota string-count 1 1))
             (string-stencils (map string-stencil string-list)))
        (apply ly:stencil-add empty-stencil string-stencils)))

    (define (string-stencil string)
      "Make a stencil for @code{string}, given the fret-diagram
      overall parameters."
      (let* ((string-coordinate (- string-count string))
             (current-string-thickness
              (* th size (string-thickness string thickness-factor)))
             (fret-half-thick (* size th 0.5))
             (string-half-thick (* current-string-thickness 0.5))
             (start-coordinates
              (stencil-coordinates
               (- fret-half-thick)
               (- (* size string-distance string-coordinate)
                  string-half-thick)))
             (end-coordinates
              (stencil-coordinates
               (+ fret-half-thick
                  (* size fret-distance (1+ (fret-count fret-range))))
               (+ string-half-thick
                  (* size string-distance string-coordinate)))))
        (ly:round-filled-box
         (ordered-cons (car start-coordinates) (car end-coordinates))
         (ordered-cons (cdr start-coordinates) (cdr end-coordinates))
         (* th size))))

    (define (draw-frets)
      "Draw the fret lines for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}.  Orientation is given by @var{orientation}."
      (let* ((fret-list (iota (1+ my-fret-count)))
             (fret-stencils (map fret-stencil fret-list)))
        (apply ly:stencil-add empty-stencil fret-stencils)))

    (define (fret-stencil fret)
      "Make a stencil for @code{fret}, given the
fret-diagram overall parameters."
      (let* ((low-string-half-thickness
              (* 0.5
                 size
                 th
                 (string-thickness string-count thickness-factor)))
             (fret-half-thickness (* 0.5 size th))
             (start-coordinates
              (stencil-coordinates
               (* fret-distance size fret)
               (- fret-half-thickness low-string-half-thickness)))
             (end-coordinates
              (stencil-coordinates
               (* fret-distance size fret)
               (* size string-distance (1- string-count)))))
        (make-line-stencil
         (* size th)
         (car start-coordinates) (cdr start-coordinates)
         (car end-coordinates) (cdr end-coordinates))))

    (define (draw-barre barre-list)
      "Create barre indications for a fret diagram"
      (let* ((low-fret (car fret-range))
             (barre-vertical-offset 0.5)
             (scale-dot-radius (* size dot-radius))
             (barre-type (assoc-get 'barre-type details 'curved))
             (barre-stils
              (map
               (lambda (barre)
                 (let* ((string1 (car barre))
                        (string2 (cadr barre))
                        (barre-fret (caddr barre))
                        (fret (1+ (- barre-fret low-fret)))
                        (barre-fret-coordinate
                         (+ (1- fret) dot-position))
                        (barre-start-string-coordinate
                         (- string-count string1))
                        (barre-end-string-coordinate
                         (- string-count string2)))
                   (cond
                    ((eq? barre-type 'straight)
                     (make-straight-line-stencil
                      barre-fret-coordinate
                      barre-start-string-coordinate
                      barre-end-string-coordinate
                      scale-dot-radius))
                    ((eq? barre-type 'curved)
                     (make-curved-barre-stencil
                      barre-fret-coordinate
                      barre-start-string-coordinate
                      barre-end-string-coordinate
                      scale-dot-radius)))))
               barre-list)))
        (apply ly:stencil-add empty-stencil barre-stils)))

    (define (make-straight-line-stencil
             fret
             start-string
             end-string
             thickness)
      "Create a straight line stencil.  Used for barre and capo."
      (let ((start-point
             (stencil-coordinates
              (* size fret-distance fret)
              (* size string-distance start-string)))
            (end-point
             (stencil-coordinates
              (* size fret-distance fret)
              (* size string-distance end-string))))
        (make-line-stencil
         thickness
         (car start-point) (cdr start-point)
         (car end-point) (cdr end-point))))

    (define (make-curved-barre-stencil
             fret-coordinate
             start-string-coordinate
             end-string-coordinate
             half-thickness)
      "Create a curved barre stencil."
      (let* ((bezier-thick 0.1)
             (bezier-height 0.5)
             (bezier-list
              (make-bezier-sandwich-list
               (* size string-distance start-string-coordinate)
               (* size string-distance end-string-coordinate)
               (* size fret-distance fret-coordinate)
               (* size bezier-height)
               (* size bezier-thick))))
        (make-bezier-sandwich-stencil
         bezier-list
         (* size bezier-thick))))

    (define (draw-dots dot-list)
      "Make dots for fret diagram."
      (let* ((scale-dot-radius (* size dot-radius))
             (scale-dot-thick (* size th))
             (default-dot-color (assoc-get 'dot-color details))
             (finger-label-padding 0.3)
             (dot-label-font-mag
              (* scale-dot-radius
                 (assoc-get 'dot-label-font-mag details 1.0)))
             (string-label-font-mag
              (* size
                 (assoc-get
                  'string-label-font-mag details
                  (cond ((or (eq? orientation 'landscape)
                             (eq? orientation 'opposing-landscape))
                         0.5)
                        (else  0.6)))))
             (dot-stils
              (map
               (lambda (dot-sublist)
                 (let* (
                        (current-string (car dot-sublist))
                        (fret (cadr dot-sublist))
                        (fret-coordinate
                         (* size fret-distance (+ (1- fret) dot-position)))
                        (string-coordinate
                         (* size string-distance (- string-count current-string)))
                        (dot-coordinates
                         (stencil-coordinates fret-coordinate string-coordinate))
                        (extent (cons (- scale-dot-radius) scale-dot-radius))
                        (parenthesized (dot-is-parenthesized dot-sublist))
                        (parenthesis-color (default-paren-color dot-sublist))
                        (inverted (dot-is-inverted dot-sublist))
                        (dot-color-is-white?
                         (or inverted
                             (and (eq? default-dot-color 'white) (not inverted))))
                        (what-color
                         (cond
                          ;; If no colors are set return #f
                          ;; This makes a general override of Grob.color affect
                          ;; dot-color as well
                          ((and (not (dot-has-color dot-sublist))
                                (not (assoc-get default-dot-color x11-color-list)))
                           #f)
                          ((and inverted
                                (not (dot-has-color dot-sublist))
                                (not (eq? default-dot-color 'white)))
                           (x11-color (or default-dot-color 'black)))
                          (dot-color-is-white?
                           (x11-color
                            (or (dot-has-color dot-sublist) 'black)))
                          ;; Other dots are colored with (in descending
                          ;; priority order)
                          ;;  - dot-color
                          ;;  - general default-dot-color
                          ;;  - black as fallback
                          (else
                           (x11-color
                            (or (dot-has-color dot-sublist)
                                default-dot-color
                                'black)))))
                        (inverted-stil
                         (lambda (color)
                           (ly:stencil-add
                            (stencil-with-color
                             (make-circle-stencil
                              scale-dot-radius scale-dot-thick #t)
                             color)
                            (stencil-with-color
                             (make-circle-stencil
                              (- scale-dot-radius (* 0.5 scale-dot-thick)) 0 #t)
                             (x11-color 'white)))))
                        (dot-stencil
                         (if dot-color-is-white?
                             (inverted-stil what-color)
                             (stencil-with-color
                              (make-circle-stencil
                               scale-dot-radius scale-dot-thick #t)
                              what-color)))
                        (final-dot-stencil
                         (if parenthesized
                             (let ((paren-color
                                    ;; If 'default-paren-color is in dot-sublist
                                    ;; and dots are not white use the overall
                                    ;; color, i.e. return #f
                                    ;; Otherwise use `what-color`
                                    (if (and parenthesis-color
                                             (not (eq? default-dot-color 'white)))
                                        #f
                                        what-color)))
                               (stencil-with-color
                                (parenthesize-stencil
                                 dot-stencil      ;; stencil
                                 (* size th 0.75) ;; half-thickness
                                 (* 0.15 size)    ;; width
                                 0                ;; angularity
                                 paren-padding    ;; padding
                                 )
                                paren-color))
                             dot-stencil))
                        (positioned-dot
                         (ly:stencil-translate final-dot-stencil dot-coordinates))
                        (finger (caddr dot-sublist))
                        (finger (if (number? finger) (number->string finger) finger)))

                 ;;;;
                   ;; the ready dot-stencil with fingering:
                 ;;;;
                   ;; -  for finger-code 'none use positioned-dot from above
                   ;; -  for finger-code 'in-dot calculate a stencil for the
                   ;;    finger, add it to final-dot-stencil and move the result
                   ;;    accordingly
                   ;; -  for finger-code 'below-string calculate a stencil for
                   ;;    the finger, move it accordingly and add the result
                   ;;    to positioned-dot from above
                   (cond ((or (eq? finger '())
                              (eq? finger-code 'none)
                              (eq? finger-code *unspecified*))
                          positioned-dot)
                         ((and (eq? finger-code 'in-dot) (not (null? finger)))
                          (let* ((finger-stil
                                  (sans-serif-stencil
                                   layout props dot-label-font-mag finger))
                                 (finger-stil-length
                                  (interval-length
                                   (ly:stencil-extent finger-stil X)))
                                 (finger-stil-height
                                  (interval-length
                                   (ly:stencil-extent finger-stil Y)))
                                 (dot-stencil-radius
                                  (/ (interval-length
                                      (ly:stencil-extent dot-stencil Y))
                                     2))
                                 (scale-factor
                                  (/ dot-stencil-radius
                                     ;; Calculate the radius of the circle
                                     ;; through the corners of the box
                                     ;; containing the finger-stil. Give it
                                     ;;  a little padding.
                                     ;; The value, (* 2 th), is my choice
                                     (+
                                      (ly:length
                                       (/ finger-stil-length 2)
                                       (/ finger-stil-height 2))
                                      (* 2 th))))
                                 (finger-label-stil
                                  (centered-stencil
                                   (ly:stencil-scale
                                    finger-stil
                                    scale-factor scale-factor))))
                            (ly:stencil-translate
                             (ly:stencil-add
                              final-dot-stencil
                              (if dot-color-is-white?
                                  (stencil-with-color
                                   finger-label-stil
                                   what-color)
                                  (stencil-with-color
                                   finger-label-stil white)))
                             dot-coordinates)))
                         ((eq? finger-code 'below-string)
                          (let* ((finger-label-stencil
                                  (centered-stencil
                                   (sans-serif-stencil
                                    layout props string-label-font-mag
                                    finger)))
                                 (finger-label-fret-offset
                                  (stencil-fretboard-offset
                                   finger-label-stencil 'fret orientation))
                                 (finger-label-fret-coordinate
                                  ;; (1) Move the below-string-finger-codes to
                                  ;;     the bottom edge of the string, i.e.
                                  ;;       (* (1+  my-fret-count) fret-distance)
                                  ;; (2) add `finger-label-padding' (a hardcoded
                                  ;;     correction-value to get a bit default
                                  ;;     padding).
                                  ;;     TODO: make it a property?
                                  ;; (3) scale this with `size'
                                  ;; (4) add `label-fret-offset', to get the
                                  ;;     final padding
                                  (+
                                   (* size
                                      (+ (* (1+  my-fret-count) fret-distance)
                                         finger-label-padding))
                                   finger-label-fret-offset))
                                 (finger-label-translation
                                  (stencil-coordinates
                                   finger-label-fret-coordinate
                                   string-coordinate)))
                            (ly:stencil-add
                             positioned-dot
                             (ly:stencil-translate
                              finger-label-stencil
                              finger-label-translation))))
                         (else
                          ;; unknown finger-code, warn
                          (ly:warning
                           "Unknown finger-code ~a, ignoring." finger-code)
                          positioned-dot))))
               dot-list)))
        (apply ly:stencil-add empty-stencil dot-stils)))

    (define (draw-thick-zero-fret)
      "Draw a thick zeroth fret for a fret diagram whose base fret is 1.
Respect changes of @code{size} and
@code{fret-diagram-details.string-thickness-factor}."
      (let* ((half-lowest-string-thickness
              (* 0.5 sth (string-thickness string-count thickness-factor)))
             (half-thick (* 0.5 sth))
             (top-fret-thick
              (* sth (assoc-get 'top-fret-thickness details 3.0)))
             (start-string-coordinate
              (- half-lowest-string-thickness))
             (end-string-coordinate
              (+ (* size string-distance (1- string-count)) half-thick))
             (start-fret-coordinate half-thick)
             (end-fret-coordinate (- half-thick top-fret-thick))
             (lower-left
              (stencil-coordinates
               start-fret-coordinate start-string-coordinate))
             (upper-right
              (stencil-coordinates
               end-fret-coordinate end-string-coordinate)))
        (ly:round-filled-box
         ;; Put limits in order, or else the intervals are considered empty
         (ordered-cons (car lower-left) (car upper-right))
         (ordered-cons (cdr lower-left) (cdr upper-right))
         sth)))

    (define (draw-xo xo-list)
      "Put open and mute string indications on diagram, as contained in
@var{xo-list}."
      (let* ((xo-font-mag (assoc-get 'xo-font-magnification details 0.4))
             (diagram-fret-top
              (car (stencil-fretboard-extent
                    fret-diagram-stencil
                    'fret
                    orientation)))
             (xo-stils
              (map
               (lambda (xo-sublist)
                 (let* ((glyph-string
                         (if (eq? (car xo-sublist) 'mute)
                             (assoc-get 'mute-string details "X")
                             (assoc-get 'open-string details "O")))
                        (glyph-string-coordinate
                         (* (- string-count (cadr xo-sublist))
                            string-distance size))
                        (glyph-stencil
                         (centered-stencil
                          (sans-serif-stencil
                           layout props
                           (* size xo-font-mag) glyph-string)))
                        (glyph-stencil-coordinates
                         (stencil-coordinates 0 glyph-string-coordinate)))
                   (ly:stencil-translate
                    glyph-stencil
                    glyph-stencil-coordinates)))
               xo-list))
             (xo-stencil (apply ly:stencil-add empty-stencil xo-stils))
             (xo-fret-offset
              (stencil-fretboard-offset xo-stencil 'fret orientation))
             (xo-stencil-offset
              (stencil-coordinate-offset
               (- diagram-fret-top xo-fret-offset (* size xo-padding))
               0)))
        (ly:stencil-translate xo-stencil xo-stencil-offset)))

    (define (draw-capo fret)
      "Draw a capo indicator across the full width of the fret-board
at @var{fret}."
      (let* ((capo-thick (* size (assoc-get 'capo-thickness details 0.5)))
             (last-string-position 0)
             (first-string-position (* size (- string-count 1)))
             (fret-position (* size (1- (+ dot-position fret)))))
        (make-straight-line-stencil
         fret-position
         last-string-position
         first-string-position
         capo-thick)))

    (define (label-fret fret-range)
      "Label the base fret on a fret diagram"
      (let* ((base-fret (car fret-range))
             (label-font-mag (assoc-get 'fret-label-font-mag details 0.5))
             (label-space (* 0.5 size))
             (label-dir (assoc-get 'label-dir details RIGHT))
             (label-vertical-offset
              (assoc-get 'fret-label-vertical-offset details 0))
             (label-horizontal-offset
              (assoc-get 'fret-label-horizontal-offset details 0))
             (number-type
              (assoc-get 'number-type details 'roman-lower))
             (label-text
              (number-format number-type base-fret
                             (assoc-get 'fret-label-custom-format
                                        details "~a")))
             (label-stencil
              (centered-stencil
               (sans-serif-stencil
                layout props (* size label-font-mag) label-text)))
             (label-half-width
              (stencil-fretboard-offset
               label-stencil
               'string
               orientation))
             (label-outside-diagram
              (+ label-space
                 (* size label-horizontal-offset)
                 label-half-width)))
        (ly:stencil-translate
         label-stencil
         (stencil-coordinates
          (* size fret-distance (1+ label-vertical-offset))
          (if (eqv? label-dir LEFT)
              (- label-outside-diagram)
              (+ (* size string-distance (1- string-count))
                 label-outside-diagram))))))
    ;;;;
    ;; Here is the body of make-fret-diagram
    ;;;;

    ;; starting with an empty stencil,
    ;; add strings and frets
    (set! fret-diagram-stencil
          (ly:stencil-add (draw-strings) (draw-frets)))

    ;; add barre(s)
    (if (and (not (null? barre-list))
             (not (eq? 'none barre-type)))
        (set! fret-diagram-stencil
              (ly:stencil-add
               (draw-barre barre-list)
               fret-diagram-stencil)))

    ;; add dots
    (if (not (null? dot-list))
        (set! fret-diagram-stencil
              (ly:stencil-add
               fret-diagram-stencil
               (draw-dots dot-list))))

    ;; add thick zero fret
    (if (= (car fret-range) 1)
        (set! fret-diagram-stencil
              (ly:stencil-add
               fret-diagram-stencil
               (draw-thick-zero-fret))))

    ;; add open/mute indicators
    (if (pair? xo-list)
        (set! fret-diagram-stencil
              (ly:stencil-add
               fret-diagram-stencil
               (draw-xo xo-list))))

    ;; add capo
    (if (> capo-fret 0)
        (set! fret-diagram-stencil
              (ly:stencil-add
               fret-diagram-stencil
               (draw-capo capo-fret))))

    ;; add fret-label
    (if (> (car fret-range) 1)
        (set! fret-diagram-stencil
              (ly:stencil-add
               fret-diagram-stencil
               (label-fret fret-range))))

    (ly:stencil-aligned-to fret-diagram-stencil X alignment)))

(define (fret-parse-definition-string props definition-string)
  "Parse a fret diagram string and return a pair containing
@var{props}, modified as necessary by the @var{definition-string},
and a fret indication list with the appropriate values."
  (let* ((fret-count 4)
         (string-count 6)
         (fret-range (cons 1 fret-count))
         (barre-list '())
         (dot-list '())
         (xo-list '())
         (output-list '())
         (new-props '())
         (details (merge-details 'fret-diagram-details props '()))
         ;; remove whitespace-characters from definition-string
         (cleared-string (remove-whitespace definition-string))
         (items (string-split cleared-string #\;)))
    (let parse-item ((myitems items))
      (if (not (null? (cdr myitems)))
          (let ((test-string (car myitems)))
            (case (car (string->list (string-take test-string 1)))
              ;; size
              ((#\s) (let ((size (get-numeric-from-key test-string)))
                       (set! props (prepend-alist-chain 'size size props))))
              ;; thickness
              ((#\t) (let ((th (get-numeric-from-key test-string)))
                       (set! props (prepend-alist-chain 'thickness th props))))
              ;; finger-code
              ((#\f) (let* ((finger-code (get-numeric-from-key test-string))
                            (finger-id (case finger-code
                                         ((0) 'none)
                                         ((1) 'in-dot)
                                         ((2) 'below-string))))
                       (set! details
                             (acons 'finger-code finger-id details))))
              ;; barre
              ((#\c) (set! output-list
                           (cons-fret
                            (cons
                             'barre
                             (numerify
                              (string-split (string-drop test-string 2) #\-)))
                            output-list)))
              ;; number of frets
              ((#\h) (let ((fret-count (get-numeric-from-key test-string)))
                       (set! details
                             (acons 'fret-count fret-count details))))
              ;; string-count
              ((#\w) (let ((string-count (get-numeric-from-key test-string)))
                       (set! details
                             (acons 'string-count string-count details))))
              ;; dot-radius
              ((#\d) (let ((dot-size (get-numeric-from-key test-string)))
                       (set! details
                             (acons 'dot-radius dot-size details))))
              ;; dot-position
              ((#\p) (let ((dot-position (get-numeric-from-key test-string)))
                       (set! details
                             (acons 'dot-position dot-position details))))
              (else
               (let* ((this-list (string-split test-string #\-))
                      (string-number (string->number (car this-list))))
                 ;; If none of the above applies, `string-number' needs to be a
                 ;; number. Throw an error, if not.
                 (cond ((not string-number)
                        (ly:error
                         "Unhandled entry in fret-diagrams \"~a\" in \"~a\""
                         (car this-list)
                         test-string))
                       ;; string-count may be modified by "w:", we need to
                       ;; look into details again, default string-count as
                       ;; fallback
                       ((> string-number
                           (assoc-get 'string-count details string-count))
                        (ly:warning (G_ "String ~a out of range 1-~a, ignoring")
                                    string-number (assoc-get 'string-count details)))
                       ((equal? (cadr this-list) "x" )
                        (set! output-list
                              (cons-fret
                               (list 'mute string-number)
                               output-list)))
                       ((equal? (cadr this-list) "o" )
                        (set! output-list
                              (cons-fret
                               (list 'open string-number)
                               output-list)))
                       (else
                        (set! output-list
                              (cons-fret
                               (cons 'place-fret (numerify this-list))
                               output-list)))))))
            (parse-item (cdr myitems)))))
    ;; add the modified details
    (set! props
          (prepend-alist-chain 'fret-diagram-details details props))
    (cons props output-list)))

(define-public
  (fret-parse-terse-definition-string props definition-string)
  "Parse a fret diagram string that uses terse syntax; return a pair containing
@var{props}, modified to include the string-count determined by
@var{definition-string}, and a fret indication list with the appropriate
values."
  ;; TODO -- change syntax to fret\string-finger

  (let* ((details (merge-details 'fret-diagram-details props '()))
         (barre-start-list '())
         (output-list '())
         (new-props '())
         (items (string-split definition-string #\;))
         (string-count (1- (length items))))
    (let parse-item ((myitems items))
      (if (not (null? (cdr myitems)))
          (let* ((test-string (car myitems))
                 (current-string (1- (length myitems)))
                 (indicators (string-split test-string #\space)))
            (let parse-indicators ((myindicators indicators))
              (if (pair? myindicators)
                  (let* ((this-list (string-split (car myindicators) #\-))
                         (last-element (last this-list))
                         (fret
                          (or (string->number (car this-list))
                              (car this-list))))
                    ;; barre-stuff
                    (if (equal? last-element "(")
                        (begin
                          (set! barre-start-list
                                (cons-fret (list current-string fret)
                                           barre-start-list))
                          (set! this-list (drop-right this-list 1))))
                    (if (equal? last-element ")")
                        (let* ((this-barre
                                (get-sub-list fret barre-start-list)))
                          (if (not this-barre)
                              (ly:warning
                               "Ignoring barre end without beginning on fret: ~a"
                               fret)
                              (set! output-list
                                    (cons-fret (cons* 'barre
                                                      (car this-barre)
                                                      current-string
                                                      (cdr this-barre))
                                               output-list)))
                          (set! this-list (drop-right this-list 1))))
                    ;; other stuff
                    (set! output-list
                          (cons-fret
                           (cond ((number? fret)
                                  (cons*
                                   'place-fret
                                   current-string
                                   (numerify this-list)))
                                 ((equal? (car this-list) "x")
                                  (list 'mute current-string))
                                 (else (list 'open current-string)))
                           output-list))
                    (parse-indicators (cdr myindicators)))))
            (parse-item (cdr myitems)))))
    (set! details (acons 'string-count string-count details))
    (set! props (prepend-alist-chain 'fret-diagram-details details props))
    (cons props output-list)))


(define-markup-command
  (fret-diagram-verbose layout props marking-list)
  (pair?) ; argument type (list, but use pair? for speed)
  #:category instrument-specific-markup ; markup type
  #:properties ((align-dir -0.4) ; properties and defaults
                (size 1.0)
                (fret-diagram-details)
                (thickness 0.5))
  "Make a fret diagram containing the symbols indicated in @var{marking-list}.

  For example,

@example
\\markup \\fret-diagram-verbose
  #'((mute 6) (mute 5) (open 4)
     (place-fret 3 2) (place-fret 2 3) (place-fret 1 2))
@end example

@noindent
produces a standard D@tie{}chord diagram without fingering indications.

Possible elements in @var{marking-list}:

@table @code
@item (mute @var{string-number})
Place a small @q{x} at the top of string @var{string-number}.

@item (open @var{string-number})
Place a small @q{o} at the top of string @var{string-number}.

@item (barre @var{start-string} @var{end-string} @var{fret-number})
Place a barre indicator (much like a tie) from string @var{start-string}
to string @var{end-string} at fret @var{fret-number}.

@item (capo @var{fret-number})
Place a capo indicator (a large solid bar) across the entire fretboard
at fret location @var{fret-number}.  Also, set fret @var{fret-number}
to be the lowest fret on the fret diagram.

@item (place-fret @var{string-number} @var{fret-number} [@var{finger-value}] [@var{color-modifier}] [@var{color}] ['parenthesized ['default-paren-color]])
Place a fret playing indication on string @var{string-number} at fret
@var{fret-number} with an optional fingering label @var{finger-value},
an optional color modifier @var{color-modifier}, an optional color
@var{color}, an optional parenthesis @code{'parenthesized} and an
optional paranthesis color @code{'default-paren-color}.
By default, the fret playing indicator is a solid dot.  This can be
globally changed by setting the value of the variable @var{dot-color}
or for a single dot by setting the value of @var{color}.  The dot can
be parenthesized by adding @code{'parenthesized}.  By default the
color for the parenthesis is taken from the dot.  Adding
@code{'default-paren-color} will take the parenthesis-color from the
global @var{dot-color}, as a fall-back black will be used.
Setting @var{color-modifier} to @code{inverted} inverts the dot color
for a specific fingering.
The values for @var{string-number}, @var{fret-number}, and the optional
@var{finger} should be entered first in that order.
The order of the other optional arguments does not matter.
If the @var{finger} part of the @code{place-fret} element is present,
@var{finger-value} will be displayed according to the setting of the
variable @var{finger-code}.  There is no limit to the number of fret
indications per string.
@end table"

  (make-fret-diagram layout props marking-list))


(define-markup-command (fret-diagram layout props definition-string)
  (string?) ; argument type
  #:category instrument-specific-markup ; markup category
  #:properties (fret-diagram-verbose-markup) ; properties and defaults
  #:as-string ""
  "Make a (guitar) fret diagram.  For example, say

@example
\\markup \\fret-diagram #\"s:0.75;6-x;5-x;4-o;3-2;2-3;1-2;\"
@end example

@noindent
for fret spacing 3/4 of staff space, D chord diagram

Syntax rules for @var{definition-string}:
@itemize @minus

@item
Diagram items are separated by semicolons.

@item
Possible items:

@itemize @bullet
@item
@code{s:}@var{number} -- Set the fret spacing of the diagram (in staff
spaces).
Default:@tie{}1.

@item
@code{t:}@var{number} -- Set the line thickness (relative to normal
line thickness).
Default:@tie{}0.5.

@item
@code{h:}@var{number} -- Set the height of the diagram in frets.
Default:@tie{}4.

@item
@code{w:}@var{number} -- Set the width of the diagram in strings.
Default:@tie{}6.

@item
@code{f:}@var{number} -- Set fingering label type
 (0@tie{}= none, 1@tie{}= in circle on string, 2@tie{}= below string).
Default:@tie{}0.

@item
@code{d:}@var{number} -- Set radius of dot, in terms of fret spacing.
Default:@tie{}0.25.

@item
@code{p:}@var{number} -- Set the position of the dot in the fret space.
0.5 is centered; 1@tie{}is on lower fret bar, 0@tie{}is on upper fret bar.
Default:@tie{}0.6.

@item
@code{c:}@var{string1}@code{-}@var{string2}@code{-}@var{fret} -- Include a
barre mark from @var{string1} to @var{string2} on @var{fret}.

@item
@var{string}@code{-}@var{fret} -- Place a dot on @var{string} at @var{fret}.
If @var{fret} is @samp{o}, @var{string} is identified as open.
If @var{fret} is @samp{x}, @var{string} is identified as muted.

@item
@var{string}@code{-}@var{fret}@code{-}@var{fingering} -- Place a dot on
@var{string} at @var{fret}, and label with @var{fingering} as defined
by the @code{f:} code.
@end itemize

@item
Note: There is no limit to the number of fret indications per string.
@end itemize"
  (let ((definition-list
          (fret-parse-definition-string props definition-string)))
    (fret-diagram-verbose-markup
     layout (car definition-list) (cdr definition-list))))

(define-markup-command
  (fret-diagram-terse layout props definition-string)
  (string?) ; argument type
  #:category instrument-specific-markup ; markup category
  #:properties (fret-diagram-verbose-markup) ; properties
  #:as-string ""
  "Make a fret diagram markup using terse string-based syntax.

Here is an example

@example
\\markup \\fret-diagram-terse #\"x;x;o;2;3;2;\"
@end example

@noindent
for a D@tie{}chord diagram.

Syntax rules for @var{definition-string}:

@itemize @bullet

@item
Strings are terminated by semicolons; the number of semicolons
is the number of strings in the diagram.

@item
Mute strings are indicated by @samp{x}.

@item
Open strings are indicated by @samp{o}.

@item
A number indicates a fret indication at that fret.

@item
If there are multiple fret indicators desired on a string, they
should be separated by spaces.

@item
Fingerings are given by following the fret number with a @w{@code{-},}
followed by the finger indicator, e.g. @samp{3-2} for playing the third
fret with the second finger.

@item
Where a barre indicator is desired, follow the fret (or fingering) symbol
with @w{@code{-(}} to start a barre and @w{@code{-)}} to end the barre.

@end itemize"
  ;; TODO -- change syntax to fret\string-finger
  (let ((definition-list
          (fret-parse-terse-definition-string props definition-string)))
    (fret-diagram-verbose-markup
     layout (car definition-list) (cdr definition-list))))
