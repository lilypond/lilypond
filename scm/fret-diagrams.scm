;;;; fret-diagrams.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2009 Carl D. Sorensen <c_sorensen@byu.edu>

;
;  Utility functions
;
;
    
(define (string-x-extent start-point end-point)
  "Return the x-extent of a string that goes from start-point
to end-point."
  (let ((x1 (car start-point))
         (x2 (car end-point)))
    (if (> x1 x2)
        (cons x2 x1)
        (cons x1 x2))))

(define (string-y-extent start-point end-point)
  "Return the y-extent of a string that goes from start-point
to end-point."
  (let ((y1 (cdr start-point))
         (y2 (cdr end-point)))
    (if (> y1 y2)
        (cons y2 y1)
        (cons y1 y2))))


(define (cons-fret new-value old-list)
  "Put together a fret-list in the format desired by parse-string"
  (if (eq? old-list '())
      (list new-value)
      (cons* new-value old-list)))

(define (get-numeric-from-key keystring)
  "Get the numeric value from a key of the form k:val"
  (string->number (substring keystring 2 (string-length keystring))))

(define (numerify mylist)
  "Convert string values to numeric or character"
  (if (null? mylist)
      '()
      (let ((numeric-value (string->number (car mylist))))
        (if numeric-value
            (cons* numeric-value (numerify (cdr mylist)))
            (cons* (car (string->list (car mylist)))
                   (numerify (cdr mylist)))))))

(define (stepmag mag)
  "Calculate the font step necessary to get a desired magnification"
  (* 6 (/ (log mag) (log 2))))

(define (fret-count fret-range)
 "Calculate the fret count for the diagram given the range of frets in the diagram."
 (1+ (- (cdr fret-range) (car fret-range))))

(define (subtract-base-fret base-fret dot-list)
  "Subtract @var{base-fret} from every fret in @var{dot-list}"
  (if (null? dot-list)
      '()
      (let ((this-list (car dot-list)))
        (cons* (list (car this-list) (- (second this-list) base-fret)
                     (if (null? (cddr this-list))
                         '()
                         (third this-list)))
               (subtract-base-fret base-fret (cdr dot-list))))))

(define (make-bezier-sandwich-list start stop base height 
         half-thickness orientation)
  "Make the argument list for a bezier sandwich from
string coordinate @var{start} to string-coordinate @var{stop} with a 
baseline at fret coordinate @var{base}, a height of
@var{height}, and a half thickness of @var{half-thickness}."
  (let* ((width (+ (- stop start) 1))
         (cp-left-width (+ (* width half-thickness) start))
         (cp-right-width (- stop (* width half-thickness)))
         (bottom-control-point-height 
           (- base (- height half-thickness)))
         (top-control-point-height
           (- base height))
         (left-end-point 
          (stencil-coordinates base start orientation))
         (right-end-point
          (stencil-coordinates base stop orientation))
         (left-upper-control-point
          (stencil-coordinates 
            top-control-point-height cp-left-width orientation))
         (left-lower-control-point
          (stencil-coordinates 
            bottom-control-point-height cp-left-width orientation))
         (right-upper-control-point
          (stencil-coordinates 
            top-control-point-height cp-right-width orientation))
         (right-lower-control-point
          (stencil-coordinates 
            bottom-control-point-height cp-right-width orientation)))
    ; order of bezier control points is:
    ;    left cp low, right cp low, right end low, left end low
    ;    right cp high, left cp high, left end high, right end high.
    ;
   (list left-lower-control-point
         right-lower-control-point
         right-end-point
         left-end-point
         right-upper-control-point
         left-upper-control-point
         left-end-point
         right-end-point)))

(define (drop-paren item-list)
  "Drop a final parentheses from a fret indication list
@code{item-list} resulting from a terse string specification of barre."
  (if (> (length item-list) 0)
      (let* ((max-index (- (length item-list) 1))
             (last-element (car (list-tail item-list max-index))))
        (if (or (equal? last-element ")") (equal? last-element "("))
            (list-head item-list max-index)
            item-list))
      item-list))

(define (get-sub-list value master-list)
  "Get a sub-list whose cadr is equal to @var{value} from @var{master-list}"
  (if (eq? master-list '())
      #f
      (let ((sublist (car master-list)))
        (if (equal? (cadr sublist) value)
            sublist
            (get-sub-list value (cdr master-list))))))

(define (merge-details key alist-list . default)
  "Return @code{alist-list} entries for @code{key}, in one combined alist.
There can be two @code{alist-list} entries for a given key. The first
comes from the override-markup function, the second comes
from property settings during a regular override.
This is necessary because some details can be set in one
place, while others are set in the other.  Both details
lists must be merged into a single alist.
Return @code{default} (optional, else #f) if not
found."

  (define (helper key alist-list default)
    (if (null? alist-list)
        default
        (let* ((handle (assoc key (car alist-list))))
          (if (pair? handle)
              (append (cdr handle) (chain-assoc-get key (cdr alist-list) '()))
              (helper key (cdr alist-list) default)))))

  (helper key alist-list
          (if (pair? default) (car default) #f)))

;
;  Conversions between fret/string coordinate system and x-y coordinate
;  system.
;  
;  Fret coordinates are measured down the fretboard from the nut,
;   starting at 0.
;
; String coordinates are measured from the lowest string, starting at 0.
;
; The x-y origin is at the intersection of the nut and the lowest string.
;
; X coordinates are positive to the right.
; Y coordinates are positive up.
;

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
        ; else -- eq? fretboard-axis 'string
  (cond ((eq? orientation 'landscape)
         (ly:stencil-extent stencil Y))
        ((eq? orientation 'opposing-landscape)
         (negate-extent (ly:stencil-extent stencil Y)))
        (else
         (ly:stencil-extent stencil Y)))))


(define (stencil-fretboard-offset stencil fretboard-axis orientation)
 "Return a the stencil coordinates of the center of @code{stencil}
in the @code{fretboard-axis} direction."
  (* 0.5 (interval-length 
           (stencil-fretboard-extent stencil fretboard-axis orientation))))

(define (stencil-coordinates fret-coordinate string-coordinate orientation)
 "Return a pair @code{(x-coordinate . y-coordinate)} in stencil coordinate 
system."
  (cond
   ((eq? orientation 'landscape)
    (cons fret-coordinate string-coordinate))
   ((eq? orientation 'opposing-landscape)
    (cons (- fret-coordinate) (- string-coordinate)))
   (else
    (cons string-coordinate (- fret-coordinate)))))
 
(define (string-thickness string thickness-factor)
  (expt (1+ thickness-factor) (1- string)))
  
;
;  Functions that create stencils used in the fret diagram
;

(define (sans-serif-stencil layout props mag text)
  "Create a stencil in sans-serif font based on @var{layout} and @var{props}
with magnification @var{mag} of the string @var{text}."
  (let* ((my-props
          (prepend-alist-chain
           'font-size (stepmag mag)
           (prepend-alist-chain 'font-family 'sans props))))
    (interpret-markup layout my-props text)))


(define (string-stencil string string-count fret-range
                        th thickness-factor size orientation)
 "Make a stencil for @code{string}, given the fret-diagram
overall parameters."
  (let* ((string-coordinate (- string-count string))
         (current-string-thickness 
           (* th size (string-thickness string thickness-factor)))
         (fret-half-thickness (* size th 0.5))
         (half-string (* current-string-thickness 0.5))
         (start-coordinates
           (stencil-coordinates
             (- fret-half-thickness)
             (- (* size string-coordinate) half-string)
             orientation))
         (end-coordinates
           (stencil-coordinates
            (+ fret-half-thickness (* size (1+ (fret-count fret-range))))
            (+ half-string (* size string-coordinate))
            orientation)))
   (ly:round-filled-box (string-x-extent start-coordinates end-coordinates)
                        (string-y-extent start-coordinates end-coordinates)
                        (* th size))))
   
(define (fret-stencil fret string-count th 
                      thickness-factor size orientation)
 "Make a stencil for @code{fret}, given the fret-diagram overall parameters."
 (let* ((low-string-half-thickness 
          (* 0.5 size th (string-thickness string-count thickness-factor)))
        (fret-half-thickness (* 0.5 size th)) 
        (start-coordinates 
         (stencil-coordinates
           (* size fret)
           (- fret-half-thickness low-string-half-thickness)
           orientation))
        (end-coordinates
         (stencil-coordinates
          (* size fret)
          (* size (1- string-count))
          orientation)))
  (make-line-stencil
   (* size th)
   (car start-coordinates) (cdr start-coordinates)
   (car end-coordinates) (cdr end-coordinates))))

(define (make-straight-barre-stencil 
          size half-thickness fret-coordinate
          start-string-coordinate end-string-coordinate orientation)
  "Create a straight barre stencil."
  (let ((start-point 
         (stencil-coordinates
          (* size fret-coordinate)
          (* size start-string-coordinate)
          orientation))
        (end-point
         (stencil-coordinates
          (* size fret-coordinate)
          (* size end-string-coordinate)
          orientation)))
   (make-line-stencil
     half-thickness
     (car start-point)
     (cdr start-point)
     (car end-point)
     (cdr end-point))))

(define (make-curved-barre-stencil 
          size half-thickness fret-coordinate
          start-string-coordinate end-string-coordinate orientation)
  "Create a curved barre stencil."
  (let* ((bezier-thick 0.1)
         (bezier-height 0.5)
         (bezier-list 
           (make-bezier-sandwich-list
            (* size start-string-coordinate)
            (* size end-string-coordinate)
            (* size fret-coordinate)
            (* size bezier-height)
            (* size bezier-thick)
            orientation))
         (box-lower-left
          (stencil-coordinates 
           (+ (* size fret-coordinate) half-thickness)
           (- (* size start-string-coordinate) half-thickness)
           orientation))
         (box-upper-right
          (stencil-coordinates
           (- (* size fret-coordinate) (* size bezier-height) half-thickness)
           (+ (* size end-string-coordinate) half-thickness)
           orientation))
         (x-extent (cons (car box-lower-left) (car box-upper-right)))
         (y-extent (cons (cdr box-lower-left) (cdr box-upper-right))))
    (ly:make-stencil
      (list 'bezier-sandwich
            `(quote ,bezier-list)
            (* size bezier-thick))
      x-extent
      y-extent)))


;
;
;  Functions used to draw fret-diagram elements
;
;

(define (draw-strings string-count fret-range th 
                      thickness-factor size orientation)
  "Draw the string lines for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}.  Orientation is determined by @var{orientation}. "

  (define (helper x)
     (if (null? (cdr x))
         (string-stencil 
          (car x) string-count fret-range th
          thickness-factor size orientation)
         (ly:stencil-add 
           (string-stencil 
            (car x) string-count fret-range th
            thickness-factor size orientation)
           (helper (cdr x)))))

  (let* ( (string-list (map 1+ (iota string-count))))
   (helper string-list)))

(define (draw-fret-lines fret-count string-count th 
                         thickness-factor size orientation)
  "Draw @var{fret-count} fret lines for a fret diagram
with @var{string-count} strings.  Line thickness is given by @var{th},
fret & string spacing by @var{size}. Orientation is given by @var{orientation}"
  (define (helper x)
     (if (null? (cdr x))
         (fret-stencil 
          (car x) string-count th thickness-factor
           size orientation)
         (ly:stencil-add 
           (fret-stencil 
            (car x) string-count th thickness-factor
            size orientation)
           (helper (cdr x)))))

  (let* ( (fret-list (iota (1+ fret-count))))
   (helper fret-list)))

(define (draw-thick-zero-fret details string-count th 
                              thickness-factor size orientation)
  "Draw a thick zeroth fret for a fret diagram whose base fret is 1."
  (let* ((sth (* th size))
         (half-lowest-string-thickness 
           (* 0.5 th (string-thickness string-count thickness-factor))) 
         (half-thick (* 0.5 sth))
         (top-fret-thick
           (* sth (assoc-get 'top-fret-thickness details 3.0)))
         (start-string-coordinate (- half-lowest-string-thickness))
         (end-string-coordinate (+ (* size (1- string-count)) half-thick))
         (start-fret-coordinate half-thick)
         (end-fret-coordinate (- half-thick top-fret-thick))
         (lower-left 
          (stencil-coordinates 
            start-fret-coordinate start-string-coordinate orientation))
         (upper-right 
          (stencil-coordinates 
            end-fret-coordinate end-string-coordinate orientation)))
   (ly:round-filled-box 
     (cons (car lower-left) (car upper-right))
     (cons (cdr lower-left) (cdr upper-right))
     sth)))
  

(define (draw-capo details string-count fret fret-count th size 
                   dot-pos orientation)
  "Draw a capo indicator across the full width of the fret-board
at @var{fret}."
(let* ((capo-thick
         (* size (assoc-get 'capo-thickness details 0.5)))
       (half-thick (* capo-thick 0.5))
       (last-string-pos 0)
       (first-string-pos (* size (- string-count 1)))
       (fret-pos ( * size (1- (+ dot-pos fret))))
       (start-point 
         (stencil-coordinates fret-pos first-string-pos orientation))
       (end-point 
         (stencil-coordinates fret-pos last-string-pos orientation)))
  (make-line-stencil
     capo-thick 
     (car start-point) (cdr start-point)
     (car end-point) (cdr end-point))))

(define (draw-frets fret-range string-count th 
                    thickness-factor size orientation)
  "Draw the fret lines for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}. Orientation is given by @var{orientation}."
  (let* ((my-fret-count (fret-count fret-range)))
   (draw-fret-lines 
     my-fret-count string-count th thickness-factor size orientation)))

(define (draw-dots layout props string-count fret-count
                   size finger-code
                   dot-position dot-radius dot-thickness dot-list orientation)
  "Make dots for fret diagram."

  (let* ((details (merge-details 'fret-diagram-details props '()))
         (scale-dot-radius (* size dot-radius))
         (scale-dot-thick (* size dot-thickness))
         (dot-color (assoc-get 'dot-color details 'black))
         (finger-label-padding 0.3)
         (dot-label-font-mag
           (* scale-dot-radius (assoc-get 'dot-label-font-mag details 1.0)))
         (string-label-font-mag
           (* size 
             (assoc-get 'string-label-font-mag details 
                        (cond ((or (eq? orientation 'landscape)
                                   (eq? orientation 'opposing-landscape))
                               0.5)
                              (else  0.6)))))
         (mypair (car dot-list))
         (restlist (cdr dot-list))
         (string (car mypair))
         (fret (cadr mypair))
         (fret-coordinate (* size (+ (1- fret) dot-position)))
         (string-coordinate (* size (- string-count string)))
         (dot-coordinates 
          (stencil-coordinates fret-coordinate string-coordinate orientation))
         (extent (cons (- scale-dot-radius) scale-dot-radius))
         (finger (caddr mypair))
         (finger (if (number? finger) (number->string finger) finger))
         (dot-stencil (if (eq? dot-color 'white)
                       (ly:stencil-add
                         (make-circle-stencil
                           scale-dot-radius scale-dot-thick #t)
                         (ly:stencil-in-color
                           (make-circle-stencil
                             (- scale-dot-radius (* 0.5 scale-dot-thick))
                             0  #t)
                           1 1 1))
                       (make-circle-stencil
                         scale-dot-radius scale-dot-thick #t)))
         (positioned-dot (ly:stencil-translate dot-stencil dot-coordinates))
         (labeled-dot-stencil 
           (cond 
             ((or (eq? finger '())(eq? finger-code 'none))
              positioned-dot)
             ((eq? finger-code 'in-dot)
              (let ((finger-label 
                     (centered-stencil
                       (sans-serif-stencil
                         layout props dot-label-font-mag finger))))
              (ly:stencil-translate
                (ly:stencil-add
                  dot-stencil
                  (if (eq? dot-color 'white)
                      finger-label
                      (ly:stencil-in-color finger-label 1 1 1)))
                dot-coordinates)))
             ((eq? finger-code 'below-string)
              (let* ((label-stencil 
                       (centered-stencil 
                         (sans-serif-stencil
                           layout props string-label-font-mag
                           finger)))
                     (label-fret-offset
                       (stencil-fretboard-offset 
                         label-stencil 'fret orientation))
                     (label-fret-coordinate 
                       (+ (* size (+ 1 fret-count finger-label-padding))
                          label-fret-offset))
                     (label-string-coordinate string-coordinate)
                     (label-translation 
                       (stencil-coordinates 
                         label-fret-coordinate
                         label-string-coordinate
                         orientation)))
                (ly:stencil-add
                  positioned-dot
                  (ly:stencil-translate label-stencil label-translation))))
             (else ;unknown finger-code
               positioned-dot))))
    (if (null? restlist)
      labeled-dot-stencil
      (ly:stencil-add
        (draw-dots
          layout props string-count fret-count size finger-code
          dot-position dot-radius dot-thickness restlist orientation)
        labeled-dot-stencil))))

(define (draw-xo 
          layout props string-count fret-range size xo-list orientation)
  "Put open and mute string indications on diagram, as contained in
@var{xo-list}."
  (let* ((details (merge-details 'fret-diagram-details props '()))
         (xo-font-mag
           (* size (assoc-get 
                    'xo-font-magnification details 
                    (cond ((or (eq? orientation 'landscape)
                            (eq? orientation 'opposing-landscape))
                           0.4)
                     (else 0.4)))))
         (mypair (car xo-list))
         (restlist (cdr xo-list))
         (glyph-string (if (eq? (car mypair) 'mute)
                         (assoc-get 'mute-string details "X")
                         (assoc-get 'open-string details "O")))
         (glyph-string-coordinate (* (- string-count (cadr mypair)) size))
         (glyph-stencil 
           (centered-stencil
             (sans-serif-stencil 
               layout props (* size xo-font-mag) glyph-string)))
         (glyph-stencil-coordinates 
           (stencil-coordinates 0 glyph-string-coordinate orientation))
         (positioned-glyph
           (ly:stencil-translate glyph-stencil glyph-stencil-coordinates)))
    (if (null? restlist)
        positioned-glyph
        (ly:stencil-add
         (draw-xo
          layout props string-count fret-range size restlist orientation)
         positioned-glyph))))

(define (draw-barre layout props string-count fret-range
                    size finger-code dot-position dot-radius
                    barre-list orientation)
  "Create barre indications for a fret diagram"
  (if (not (null? barre-list))
    (let* ((details (merge-details 'fret-diagram-details props '()))
           (string1 (caar barre-list))
           (string2 (cadar barre-list))
           (barre-fret (caddar barre-list))
           (top-fret (cdr fret-range))
           (low-fret (car fret-range))
           (fret (1+ (- barre-fret low-fret)))
           (barre-vertical-offset 0.5)
           (dot-center-fret-coordinate (+ (1- fret) dot-position))
           (barre-fret-coordinate
             (+ dot-center-fret-coordinate
                (* (- barre-vertical-offset 0.5) dot-radius)))
           (barre-start-string-coordinate (- string-count string1))
           (barre-end-string-coordinate (- string-count string2))
           (scale-dot-radius (* size dot-radius))
           (barre-type (assoc-get 'barre-type details 'curved))
           (barre-stencil
             (cond 
               ((eq? barre-type 'straight)
                (make-straight-barre-stencil 
                  size scale-dot-radius 
                  barre-fret-coordinate barre-start-string-coordinate
                  barre-end-string-coordinate orientation))
               ((eq? barre-type 'curved)
                (make-curved-barre-stencil 
                  size scale-dot-radius
                  barre-fret-coordinate barre-start-string-coordinate
                  barre-end-string-coordinate orientation)))))
      (if (not (null? (cdr barre-list)))
        (ly:stencil-add
          barre-stencil
          (draw-barre layout props string-count fret-range size finger-code
                      dot-position dot-radius (cdr barre-list) orientation))
        barre-stencil ))))

(define (label-fret layout props string-count fret-range size orientation)
  "Label the base fret on a fret diagram"
  (let* ((details (merge-details 'fret-diagram-details props '()))
         (base-fret (car fret-range))
         (label-font-mag (assoc-get 'fret-label-font-mag details 0.5))
         (label-space (* 0.5 size))
         (label-dir (assoc-get 'label-dir details RIGHT))
         (label-vertical-offset
           (assoc-get 'fret-label-vertical-offset details 0))
         (number-type
           (assoc-get 'number-type details 'roman-lower))
         (label-text
           (cond
             ((equal? number-type 'roman-lower)
              (fancy-format #f "~(~@r~)" base-fret))
             ((equal? number-type 'roman-upper)
              (fancy-format #f "~@r" base-fret))
             ((equal? 'arabic number-type)
              (fancy-format #f "~d" base-fret))
             (else (fancy-format #f "~(~@r~)" base-fret))))
         (label-stencil
           (centered-stencil
             (sans-serif-stencil 
               layout props (* size label-font-mag) label-text)))
         (label-half-width 
           (stencil-fretboard-offset label-stencil 'string orientation))
         (label-outside-diagram (+ label-space label-half-width)))
    (ly:stencil-translate
      label-stencil
      (stencil-coordinates 
        (1+ (* size label-vertical-offset))
        (if (eq? label-dir LEFT)
            (- label-outside-diagram)
            (+ (* size (1- string-count)) label-outside-diagram))
        orientation))))

;;
;;
;;  markup commands and associated functions
;;
;;
;;

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
              (set! barre-list (cons* (cdr my-item) barre-list)))
             ((eq? my-code 'capo)
               (set! capo-fret (cadr my-item)))
             ((eq? my-code 'place-fret)
              (set! dot-list (cons* (cdr my-item) dot-list))))
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
      (if (> maxfret my-fret-count)
          (set! fret-range
                (cons minfret
                      (let ((upfret (- (+ minfret my-fret-count) 1)))
                        (if (> maxfret upfret) maxfret upfret)))))
      (set! capo-fret (1+ (- capo-fret minfret)))
      ; subtract fret from dots
      (set! dot-list (subtract-base-fret (- (car fret-range) 1) dot-list)))
    (acons 'fret-range fret-range
           (acons 'barre-list barre-list
                  (acons 'dot-list dot-list
                         (acons 'xo-list xo-list 
                                (acons 'capo-fret capo-fret '())))))))

(define (make-fret-diagram layout props marking-list)
  "Make a fret diagram markup"
  (let* (
         ; note: here we get items from props that are needed in this routine,
         ; or that are needed in more than one of the procedures
         ; called from this routine.  If they're only used in one of the
         ; sub-procedure, they're obtained in that procedure
         (size (chain-assoc-get 'size props 1.0)) ; needed for everything
;TODO -- get string-count directly from length of stringTunings;
;         from FretBoard engraver, but not from markup call
         (details (merge-details 'fret-diagram-details props '()))
         (string-count
          (assoc-get 'string-count details 6)) ; needed for everything
         (my-fret-count
          (assoc-get 'fret-count details 4)) ; needed for everything
         (orientation
          (assoc-get 'orientation details 'normal)) ; needed for everything
         (finger-code
          (assoc-get
           'finger-code details 'none)) ; needed for draw-dots and draw-barre
         (default-dot-radius
           (if (eq? finger-code 'in-dot) 0.425 0.25)) ; bigger dots if labeled
         (default-dot-position
           (if (eq? finger-code 'in-dot)
               (- 0.95 default-dot-radius)
               0.6)) ; move up to make room for bigger if labeled
         (dot-radius
          (assoc-get
           'dot-radius details default-dot-radius))  ; needed for draw-dots
                                                     ; and draw-barre
         (dot-position
          (assoc-get
           'dot-position details default-dot-position)) ; needed for draw-dots
                                                        ; and draw-barre
         (th
          (* (ly:output-def-lookup layout 'line-thickness)
             (chain-assoc-get 'thickness props 0.5))) ; needed for draw-frets
                                                      ; and draw-strings
         (thickness-factor (assoc-get 'string-thickness-factor details 0))
         (alignment
          (chain-assoc-get 'align-dir props -0.4)) ; needed only here
         (xo-padding
          (* size (assoc-get 'xo-padding details 0.2))) ; needed only here
         (parameters (fret-parse-marking-list marking-list my-fret-count))
         (capo-fret (assoc-get 'capo-fret parameters 0))
         (dot-list (cdr (assoc 'dot-list parameters)))
         (xo-list (cdr (assoc 'xo-list parameters)))
         (fret-range (cdr (assoc 'fret-range parameters)))
         (my-fret-count (fret-count fret-range))
         (barre-list (cdr (assoc 'barre-list parameters)))
         (barre-type
          (assoc-get 'barre-type details 'curved))
         (fret-diagram-stencil
          (ly:stencil-add
           (draw-strings 
             string-count fret-range th thickness-factor size orientation)
           (draw-frets 
             fret-range string-count th thickness-factor size orientation))))
    (if (and (not (null? barre-list))
             (not (eq? 'none barre-type)))
        (set! fret-diagram-stencil
              (ly:stencil-add
               (draw-barre layout props string-count fret-range size
                           finger-code dot-position dot-radius
                           barre-list orientation)
               fret-diagram-stencil)))
    (if (not (null? dot-list))
        (set! fret-diagram-stencil
              (ly:stencil-add
               fret-diagram-stencil
               (draw-dots layout props string-count my-fret-count 
                          size finger-code dot-position dot-radius
                          th dot-list orientation))))
    (if (= (car fret-range) 1)
        (set! fret-diagram-stencil
                  (ly:stencil-add
                     fret-diagram-stencil 
                     (draw-thick-zero-fret
                       details string-count th 
                       thickness-factor size orientation))))
    (if (not (null? xo-list))
     (let* ((diagram-fret-top 
              (car (stencil-fretboard-extent
                     fret-diagram-stencil
                     'fret
                     orientation)))
            (xo-stencil 
              (draw-xo layout props string-count fret-range
                       size xo-list orientation))
            (xo-fret-offset
              (stencil-fretboard-offset
                xo-stencil 'fret orientation)))
      (set! fret-diagram-stencil
        (ly:stencil-add
          fret-diagram-stencil
          (ly:stencil-translate
            xo-stencil
            (stencil-coordinates
             (- diagram-fret-top
                xo-fret-offset
                (* size xo-padding))
             0 ; no string offset
             orientation))))))
               
    (if (> capo-fret 0)
        (set! fret-diagram-stencil
              (ly:stencil-add
                fret-diagram-stencil
                (draw-capo details string-count capo-fret my-fret-count
                           th size dot-position orientation))))
    (if (> (car fret-range) 1)
      (set! fret-diagram-stencil
        (ly:stencil-add
           fret-diagram-stencil
           (label-fret 
             layout props string-count fret-range size orientation))))
      (ly:stencil-aligned-to fret-diagram-stencil X alignment)))

(define (fret-parse-definition-string props definition-string)
 "Parse a fret diagram string and return a pair containing:
@var{props}, modified as necessary by the definition-string
a fret-indication list with the appropriate values"
 (let* ((fret-count 4)
        (string-count 6)
        (fret-range (cons 1 fret-count))
        (barre-list '())
        (dot-list '())
        (xo-list '())
        (output-list '())
        (new-props '())
        (details (merge-details 'fret-diagram-details props '()))
        (items (string-split definition-string #\;)))
   (let parse-item ((myitems items))
     (if (not (null? (cdr myitems)))
         (let ((test-string (car myitems)))
           (case (car (string->list (substring test-string 0 1)))
             ((#\s) (let ((size (get-numeric-from-key test-string)))
                      (set! props (prepend-alist-chain 'size size props))))
             ((#\f) (let* ((finger-code (get-numeric-from-key test-string))
                           (finger-id (case finger-code
                                        ((0) 'none)
                                        ((1) 'in-dot)
                                        ((2) 'below-string))))
                      (set! details
                            (acons 'finger-code finger-id details))))
             ((#\c) (set! output-list
                          (cons-fret
                           (cons
                            'barre
                            (numerify
                             (string-split (substring test-string 2) #\-)))
                           output-list)))
             ((#\h) (let ((fret-count (get-numeric-from-key test-string)))
                      (set! details
                            (acons 'fret-count fret-count details))))
             ((#\w) (let ((string-count (get-numeric-from-key test-string)))
                      (set! details
                            (acons 'string-count string-count details))))
             ((#\d) (let ((dot-size (get-numeric-from-key test-string)))
                      (set! details
                            (acons 'dot-radius dot-size details))))
             ((#\p) (let ((dot-position (get-numeric-from-key test-string)))
                      (set! details
                            (acons 'dot-position dot-position details))))
             (else
              (let ((this-list (string-split test-string #\-)))
                (if (string->number (cadr this-list))
                    (set! output-list
                          (cons-fret
                           (cons 'place-fret (numerify this-list))
                           output-list))
                    (if (equal? (cadr this-list) "x" )
                        (set! output-list
                              (cons-fret
                               (list 'mute (string->number (car this-list)))
                               output-list))
                        (set! output-list
                              (cons-fret
                               (list 'open (string->number (car this-list)))
                               output-list)))))))
           (parse-item (cdr myitems)))))
   ;  add the modified details
   (set! props
         (prepend-alist-chain 'fret-diagram-details details props))
   `(,props . ,output-list))) ;ugh -- hard-coded spell -- procedure better

(define-public 
  (fret-parse-terse-definition-string props definition-string)
  "Parse a fret diagram string that uses terse syntax; 
return a pair containing:
@var{props}, modified to include the string-count determined by the
definition-string, and
a fret-indication list with the appropriate values"
;TODO -- change syntax to fret\string-finger

  (let* ((details (merge-details 'fret-diagram-details props '()))
         (barre-start-list '())
         (output-list '())
         (new-props '())
         (items (string-split definition-string #\;))
         (string-count (- (length items) 1)))
    (let parse-item ((myitems items))
      (if (not (null? (cdr myitems)))
          (let* ((test-string (car myitems))
                 (current-string (- (length myitems) 1))
                 (indicators (string-split test-string #\ )))
            (let parse-indicators ((myindicators indicators))
              (if (not (eq? '() myindicators))
                  (let* ((this-list (string-split (car myindicators) #\-))
                         (max-element-index (- (length this-list) 1))
                         (last-element
                          (car (list-tail this-list max-element-index)))
                         (fret
                          (if (string->number (car this-list))
                              (string->number (car this-list))
                              (car this-list))))
                    (if (equal? last-element "(")
                        (begin
                          (set! barre-start-list
                                (cons-fret (list current-string fret)
                                           barre-start-list))
                          (set! this-list
                                (list-head this-list max-element-index))))
                    (if (equal? last-element ")")
                        (let* ((this-barre
                                (get-sub-list fret barre-start-list))
                               (insert-index (- (length this-barre) 1)))
                          (set! output-list
                                (cons-fret (cons* 'barre
                                                  (car this-barre)
                                                  current-string
                                                  (cdr this-barre))
                                           output-list))
                          (set! this-list
                                (list-head this-list max-element-index))))
                    (if (number? fret)
                        (set!
                         output-list
                         (cons-fret (cons*
                                     'place-fret
                                     current-string
                                     (drop-paren (numerify this-list)))
                                    output-list))
                        (if (equal? (car this-list) "x" )
                            (set!
                             output-list
                             (cons-fret
                              (list 'mute current-string)
                              output-list))
                            (set!
                             output-list
                             (cons-fret
                              (list 'open current-string)
                              output-list))))
                    (parse-indicators (cdr myindicators)))))
            (parse-item (cdr myitems)))))
    (set! details (acons 'string-count string-count details))
    (set! props (prepend-alist-chain 'fret-diagram-details details props))
    `(,props . ,output-list))) ; ugh -- hard coded; proc is better


(define-builtin-markup-command 
  (fret-diagram-verbose layout props marking-list)
  (pair?) ; argument type (list, but use pair? for speed)
  instrument-specific-markup ; markup type
  ((align-dir -0.4) ; properties and defaults
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

@item (place-fret @var{string-number} @var{fret-number} @var{finger-value})
Place a fret playing indication on string @var{string-number} at fret
@var{fret-number} with an optional fingering label @var{finger-value}.
By default, the fret playing indicator is a solid dot.  This can be
changed by setting the value of the variable @var{dot-color}.  If the
@var{finger} part of the @code{place-fret} element is present,
@var{finger-value} will be displayed according to the setting of the
variable @var{finger-code}.  There is no limit to the number of fret
indications per string.
@end table"

  (make-fret-diagram layout props marking-list))


(define-builtin-markup-command (fret-diagram layout props definition-string)
  (string?) ; argument type
  instrument-specific-markup ; markup category
  (fret-diagram-verbose-markup) ; properties and defaults
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
@code{t:}@var{number} -- Set the line thickness (in staff spaces).
Default:@tie{}0.05.

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

(define-builtin-markup-command
  (fret-diagram-terse layout props definition-string)
  (string?) ; argument type
  instrument-specific-markup ; markup category
  (fret-diagram-verbose-markup) ; properties
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
Fingerings are given by following the fret number with a @code{-},
followed by the finger indicator, e.g. @samp{3-2} for playing the third
fret with the second finger.

@item
Where a barre indicator is desired, follow the fret (or fingering) symbol
with @code{-(} to start a barre and @code{-)} to end the barre.

@end itemize"
  ;; TODO -- change syntax to fret\string-finger
  (let ((definition-list
          (fret-parse-terse-definition-string props definition-string)))
    (fret-diagram-verbose-markup layout
                                 (car definition-list)
                                 (cdr definition-list))))


