;;;; fret-diagrams.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2008 Carl D. Sorensen <c_sorensen@byu.edu>

(define (fret-parse-marking-list marking-list fret-count)
  (let* ((fret-range (list 1 fret-count))
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
             ((eq? my-code 'place-fret)
              (set! dot-list (cons* (cdr my-item) dot-list))))
            (parse-item (cdr mylist)))))
    ;; calculate fret-range
    (let ((maxfret 0) (minfret 99))
      (let updatemax ((fret-list dot-list))
        (if (null? fret-list)
            '()
            (let ((fretval (second (car fret-list))))
              (if (> fretval maxfret) (set! maxfret fretval))
              (if (< fretval minfret) (set! minfret fretval))
              (updatemax (cdr fret-list)))))
      (if (> maxfret fret-count)
          (set! fret-range
                (list minfret
                      (let ((upfret (- (+ minfret fret-count) 1)))
                        (if (> maxfret upfret) maxfret upfret)))))
      ; subtract fret from dots
      (set! dot-list (subtract-base-fret (- (car fret-range) 1) dot-list)))
    (acons 'fret-range fret-range
           (acons 'barre-list barre-list
                  (acons 'dot-list dot-list
                         (acons 'xo-list xo-list '()))))))

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

(define (sans-serif-stencil layout props mag text)
  "Create a stencil in sans-serif font based on @var{layout} and @var{props}
with magnification @var{mag} of the string @var{text}."
  (let* ((my-props
          (prepend-alist-chain
           'font-size (stepmag mag)
           (prepend-alist-chain 'font-family 'sans props))))
    (interpret-markup layout my-props text)))

(define (draw-strings string-count fret-range th size orientation)
  "Draw the string lines for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}.  Orientation is determined by @var{orientation}. "
  (let* ((fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (sl (* (+ fret-count 1) size))
         (sth (* size th))
         (half-thickness (* sth 0.5))
         (gap (- size sth))
         (string-stencil
          (if (eq? orientation 'normal)
              (ly:make-stencil
               (list 'draw-line sth 0 0 0 sl)
               (cons (- half-thickness) half-thickness)
               (cons (- half-thickness) (+ sl half-thickness)))
              (ly:make-stencil
               (list 'draw-line sth 0 0 sl 0)
               (cons (- half-thickness) (+ sl half-thickness))
               (cons (- half-thickness) half-thickness)))))
    (if (= string-count 1)
        string-stencil
        (if (eq? orientation 'normal)
            (ly:stencil-combine-at-edge
             (draw-strings (- string-count 1) fret-range th size orientation)
             X RIGHT
             string-stencil
             gap)
            (ly:stencil-combine-at-edge
             (draw-strings (- string-count 1) fret-range th size orientation)
             Y UP
             string-stencil
             gap)))))

(define (draw-fret-lines fret-count string-count th size orientation)
  "Draw @var{fret-count} fret lines for a fret diagram
with @var{string-count} strings.  Line thickness is given by @var{th},
fret & string spacing by @var{size}. Orientation is given by @var{orientation}"
  (let* ((sth (* size th))
         (gap (- size sth))
         (fret-line (draw-fret-line string-count th size orientation)))
    (if (= fret-count 1)
        fret-line
        (if (eq? orientation 'normal)
            (ly:stencil-combine-at-edge
             (draw-fret-lines
              (- fret-count 1) string-count th size orientation)
             Y UP
             fret-line
             gap 0)
            (ly:stencil-combine-at-edge
             (draw-fret-lines
              (- fret-count 1) string-count th size orientation)
             X RIGHT
             fret-line
             gap 0)))))

(define (draw-fret-line string-count th size orientation)
  "Draw a fret line for a fret diagram."
  (let* ((fret-length (* (- string-count 1) size))
         (sth (* size th))
         (half-thickness (* sth 0.5)))
    (if (eq? orientation 'normal)
        (ly:make-stencil
         (list 'draw-line sth half-thickness size
               (- fret-length half-thickness) size)
         (cons 0 fret-length)
         (cons (- half-thickness) half-thickness))
        (ly:make-stencil
         (list 'draw-line sth 0 half-thickness
               0 (- fret-length half-thickness))
         (cons (- half-thickness) half-thickness)
         (cons 0 fret-length)))))

(define (draw-thick-zero-fret details string-count th size orientation)
  "Draw a thick zeroth fret for a fret diagram whose base fret is not 1."
  (let* ((sth (* th size))
         (top-fret-thick
          (* sth (assoc-get 'top-fret-thickness details 3.0)))
         (half-thick (* sth 0.5))
         (x1 half-thick)
         (x2 (+ half-thick (* size (- string-count 1))))
         (y1 (- half-thick))
         (y2 (+ top-fret-thick half-thick))
         (x-extent (cons (- x1) x2))
         (y-extent (cons sth top-fret-thick)))
    (if (eq? orientation 'normal)
        (ly:make-stencil (list 'round-filled-box x1 x2 y1 y2 sth)
                         x-extent y-extent)
        (ly:make-stencil (list 'round-filled-box y1 y2 x1 x2 sth)
                         y-extent x-extent))))

(define (draw-frets fret-range string-count th size orientation)
  "Draw the fret lines for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}. Orientation is given by @var{orientation}."
  (let* ((fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (fret-length (* (- string-count 1) size))
         (half-thickness (* th 0.5))
         (base-fret (car fret-range))
         (fret-zero (draw-fret-line string-count th size orientation)))
    (if (eq? orientation 'normal)
        (ly:stencil-combine-at-edge
         (draw-fret-lines fret-count string-count th size orientation)
         Y UP
         fret-zero
         (- size th))
        (ly:stencil-combine-at-edge
         fret-zero X RIGHT
         (draw-fret-lines fret-count string-count th size orientation)
         (- size th)))))

(define (draw-dots layout props string-count fret-count
                   fret-range size finger-code
                   dot-position dot-radius dot-thickness dot-list orientation)
  "Make dots for fret diagram."

  (let* ((details (chain-assoc-get 'fret-diagram-details props '()))
         (scale-dot-radius (* size dot-radius))
         (scale-dot-thick (* size dot-thickness))
         (dot-color (assoc-get 'dot-color details 'black))
         (finger-xoffset -0.25)
         (finger-yoffset (* -0.5 size ))
         (dot-label-font-mag
          (* scale-dot-radius (assoc-get 'dot-label-font-mag details 1.0)))
         (string-label-font-mag
          (* size (assoc-get 'string-label-font-mag details 0.6)))
         (mypair (car dot-list))
         (restlist (cdr dot-list))
         (string (car mypair))
         (fret (cadr mypair))
         (xpos (* size (if (eq? orientation 'normal)
                           (- string-count string)
                           (+ (- fret 1 ) dot-position))))
         (ypos (* size (if (eq? orientation 'normal)
                           (+ 2 (- fret-count fret dot-position ))
                           (- string-count string))))
         (extent (cons (- scale-dot-radius) scale-dot-radius))
         (finger (caddr mypair))
         (finger (if (number? finger) (number->string finger) finger))
         (dotstencil (if (eq? dot-color 'white)
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
         (positioned-dot (begin
                           (ly:stencil-translate-axis
                            (ly:stencil-translate-axis dotstencil xpos X)
                            ypos Y)))
         (labeled-dot-stencil
          (if (or (eq? finger '())(eq? finger-code 'none))
              positioned-dot
              (if (eq? finger-code 'in-dot)
                  (let* ((finger-label
                          (centered-stencil
                           (sans-serif-stencil
                            layout props dot-label-font-mag finger))))
                    (ly:stencil-translate-axis
                     (ly:stencil-translate-axis
                      (ly:stencil-add
                       dotstencil
                       (if (eq? dot-color 'white)
                           finger-label
                           (ly:stencil-in-color finger-label 1 1 1)))
                      xpos X)
                     ypos Y))
                  (if (eq? finger-code 'below-string)
                      (ly:stencil-add
                       positioned-dot
                       (if (eq? orientation 'normal)
                           (ly:stencil-translate-axis
                            (ly:stencil-translate-axis
                             (centered-stencil
                              (sans-serif-stencil
                               layout props string-label-font-mag finger))
                             xpos X)
                            (* size finger-yoffset) Y)
                           (ly:stencil-translate-axis
                            (ly:stencil-translate-axis
                             (centered-stencil
                              (sans-serif-stencil
                               layout props string-label-font-mag finger))
                             (* size (+ 2 fret-count finger-yoffset)) X)
                            ypos Y)))
                      ;unknown finger-code
                      positioned-dot)))))
    (if (null? restlist)
        labeled-dot-stencil
        (ly:stencil-add
         (draw-dots
          layout props string-count fret-count fret-range size finger-code
          dot-position dot-radius dot-thickness restlist orientation)
         labeled-dot-stencil))))

(define (draw-xo layout props string-count fret-range size xo-list orientation)
  "Put open and mute string indications on diagram, as contained in
@var{xo-list}."
  (let* ((details (chain-assoc-get 'fret-diagram-details props '()))
         (fret-count (+ (- (cadr fret-range) (car fret-range) 1)))
         (xo-font-mag
          (* size (assoc-get 'xo-font-magnification details 0.5)))
         (xo-horizontal-offset (* size -0.35))
         (mypair (car xo-list))
         (restlist (cdr xo-list))
         (glyph-string (if (eq? (car mypair) 'mute)
                           (assoc-get 'mute-string details "X")
                           (assoc-get 'open-string details "O")))
         (xpos
          (+ (* (- string-count (cadr mypair)) size) xo-horizontal-offset ))
         (glyph-stencil (if (eq? orientation 'normal)
                            (ly:stencil-translate-axis
                             (sans-serif-stencil
                              layout props (* size xo-font-mag) glyph-string)
                             xpos X)
                            (ly:stencil-translate-axis
                             (sans-serif-stencil
                              layout props (* size xo-font-mag) glyph-string)
                             xpos Y))))
    (if (null? restlist)
        glyph-stencil
        (ly:stencil-add
         (draw-xo
          layout props string-count fret-range size restlist orientation)
         glyph-stencil))))

(define (make-bezier-sandwich-list start stop base height thickness orientation)
  "Make the argument list for a bezier sandwich from
@var{start} to @var{stop} with a baseline at @var{base}, a height of
@var{height}, and a thickness of @var{thickness}.  If @var{orientation} is
@var{'normal}, @var{base} is a y coordinate, otherwise it's an x coordinate."
  (let* ((width (+ (- stop start) 1))
         (x1 (+ (* width thickness) start))
         (x2 (- stop (* width thickness)))
         (bottom-control-point-height
          (if (eq? orientation 'normal)
              (+ base (- height thickness))
              (- base (- height thickness))))
         (top-control-point-height
          (if (eq? orientation 'normal)
	      (+ base height)
	      (- base height))))
 ; order of bezier control points is:
 ;    left cp low, right cp low, right end low, left end low
 ;    right cp high, left cp high, left end high, right end high.
    (if (eq? orientation 'normal)
	(list (cons x1 bottom-control-point-height)
	      (cons x2 bottom-control-point-height)
	      (cons stop base)
	      (cons start base)
	      (cons x2 top-control-point-height)
	      (cons x1 top-control-point-height)
	      (cons start base)
	      (cons stop base))
	(list (cons bottom-control-point-height x1)
	      (cons bottom-control-point-height x2)
	      (cons base stop)
	      (cons base start)
	      (cons top-control-point-height x2)
	      (cons top-control-point-height x1)
	      (cons base start)
	      (cons base stop)))))

(define (draw-barre layout props string-count fret-range
		    size finger-code dot-position dot-radius
		    barre-list orientation)
  "Create barre indications for a fret diagram"
  (if (not (null? barre-list))
      (let* ((details (chain-assoc-get 'fret-diagram-details props '()))
	     (string1 (caar barre-list))
	     (string2 (cadar barre-list))
	     (fret (caddar barre-list))
	     (top-fret (cadr fret-range))
	     (low-fret (car fret-range))
	     (barre-type (assoc-get 'barre-type details 'curved))
	     (scale-dot-radius (* size dot-radius))
	     (barre-vertical-offset 0.5)
	     ;; 2 is 1 for empty fret at bottom of figure + 1 for interval
	     ;; (top-fret - fret + 1) -- not an arbitrary constant
	     (dot-center-y
	      (* size (- (+ 2 (- (cadr fret-range) fret)) dot-position)))
	     (dot-center-fret-coordinate (+ (- fret low-fret) dot-position))
	     (barre-fret-coordinate
	      (+ dot-center-fret-coordinate
		 (* (- barre-vertical-offset 0.5) dot-radius)))
	     (barre-start-string-coordinate (- string-count string1))
	     (barre-end-string-coordinate (- string-count string2))
	     (bottom
	      (+ dot-center-y (* barre-vertical-offset scale-dot-radius)))
	     (left (* size (- string-count string1)))
	     (right (* size (- string-count string2)))
	     (bezier-thick 0.1)
	     (bezier-height 0.5)
	     (bezier-list
	      (if (eq? orientation 'normal)
		  (make-bezier-sandwich-list
		   (* size barre-start-string-coordinate)
		   (* size barre-end-string-coordinate)
                   (* size (+ 2 (- top-fret 
                                   (+ low-fret barre-fret-coordinate))))
		   (* size bezier-height)
		   (* size bezier-thick)
		   orientation)
		  (make-bezier-sandwich-list
		   (* size barre-start-string-coordinate)
		   (* size barre-end-string-coordinate)
		   (* size barre-fret-coordinate)
		   (* size bezier-height)
		   (* size bezier-thick)
		   orientation)))
             (barre-stencil
              (if (eq? barre-type 'straight)
                  (if (eq? orientation 'normal)
                      (ly:make-stencil
                       (list
                        'draw-line (* size dot-radius) left dot-center-y
                        right dot-center-y)
                       (cons left right)
                       (cons (- dot-center-y scale-dot-radius)
                             (+ dot-center-y scale-dot-radius)))
                      (ly:make-stencil
                       (list 'draw-line (* size dot-radius)
                             (* size barre-fret-coordinate)
                             (* size barre-start-string-coordinate)
                             (* size barre-fret-coordinate)
                             (* size barre-end-string-coordinate))
                       (cons (- (* size barre-fret-coordinate)
                                scale-dot-radius)
                             (+ (* size barre-fret-coordinate)
                                scale-dot-radius))
                       (cons (* size barre-start-string-coordinate)
                             (* size barre-end-string-coordinate))))
                  (if (eq? orientation 'normal)
                      (ly:make-stencil
                       (list 'bezier-sandwich
                             `(quote ,bezier-list)
                             (* size bezier-thick))
                       (cons left right)
                       (cons bottom (+ bottom (* size bezier-height))))
                      (ly:make-stencil
                       (list 'bezier-sandwich
                             `(quote ,bezier-list)
                             (* size bezier-thick))
                       (cons bottom (+ bottom (* size bezier-height)))
                       (cons left right))))))
        (if (not (null? (cdr barre-list)))
            (ly:stencil-add
	     barre-stencil
	     (draw-barre layout props string-count fret-range size finger-code
			 dot-position dot-radius (cdr barre-list)))
            barre-stencil ))))

(define (stepmag mag)
  "Calculate the font step necessary to get a desired magnification"
  (* 6 (/ (log mag) (log 2))))

(define (label-fret layout props string-count fret-range size orientation)
  "Label the base fret on a fret diagram"
  (let* ((details (chain-assoc-get 'fret-diagram-details props '()))
	 (base-fret (car fret-range))
	 (label-font-mag (assoc-get 'fret-label-font-mag details 0.5))
	 (label-vertical-offset
	  (assoc-get 'fret-label-vertical-offset details -0.2))
	 (number-type (assoc-get 'number-type details 'roman-lower))
	 (fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
	 (label-text
	  (cond
           ((equal? number-type 'roman-lower)
	    (fancy-format #f "~(~@r~)" base-fret))
           ((equal? number-type 'roman-upper)
	    (fancy-format #f "~@r" base-fret))
           ((equal? 'arabic number-type)
	    (fancy-format #f "~d" base-fret))
           (else (fancy-format #f "~(~@r~)" base-fret)))))
    (if (eq? orientation 'normal)
	(ly:stencil-translate-axis
	 (sans-serif-stencil layout props (* size label-font-mag) label-text)
	 (* size (+ fret-count label-vertical-offset)) Y)
	(ly:stencil-translate-axis
	 (sans-serif-stencil layout props (* size label-font-mag) label-text)
	 (* size (+ 1 label-vertical-offset)) X))))

(define-builtin-markup-command (fret-diagram-verbose layout props marking-list)
  (list?) ; argument type
  fret-diagram ; markup type
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
;TODO -- adjust padding for fret label?  it appears to be too close to dots
         (details
	  (chain-assoc-get
	   'fret-diagram-details props '())) ; fret diagram details
         (string-count
	  (assoc-get 'string-count details 6)) ; needed for everything
         (fret-count
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
         (alignment
	  (chain-assoc-get 'align-dir props -0.4)) ; needed only here
         (xo-padding
	  (* size (assoc-get 'xo-padding details 0.2))) ; needed only here
         (label-space (* 0.25 size))
         (label-dir (assoc-get 'label-dir details RIGHT))
         (parameters (fret-parse-marking-list marking-list fret-count))
         (dot-list (cdr (assoc 'dot-list parameters)))
         (xo-list (cdr (assoc 'xo-list parameters)))
         (fret-range (cdr (assoc 'fret-range parameters)))
         (barre-list (cdr (assoc 'barre-list parameters)))
         (barre-type
          (assoc-get 'barre-type details 'curved))
         (fret-diagram-stencil
	  (ly:stencil-add
	   (draw-strings string-count fret-range th size orientation)
	   (draw-frets fret-range string-count th size orientation))))
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
	       (draw-dots layout props string-count fret-count fret-range
			  size finger-code dot-position dot-radius
			  th dot-list orientation))))
    (if (= (car fret-range) 1)
	(set! fret-diagram-stencil
	      (if (eq? orientation 'normal)
		  (ly:stencil-combine-at-edge
		   fret-diagram-stencil Y UP
		   (draw-thick-zero-fret
		    props string-count th size orientation))
		  (ly:stencil-combine-at-edge
		   fret-diagram-stencil X LEFT
		   (draw-thick-zero-fret
		    props string-count th size orientation)))))
    (if (not (null? xo-list))
	(set! fret-diagram-stencil
	      (if (eq? orientation 'normal)
		  (ly:stencil-combine-at-edge
		   fret-diagram-stencil Y UP
		   (draw-xo layout props string-count fret-range
			    size xo-list orientation)
		   xo-padding )
		  (ly:stencil-combine-at-edge
		   fret-diagram-stencil X LEFT
		   (draw-xo layout props string-count fret-range
			    size xo-list orientation)
		   xo-padding))))
    (if (> (car fret-range) 1)
	(set! fret-diagram-stencil
	      (if (eq? orientation 'normal)
		  (ly:stencil-combine-at-edge
		   fret-diagram-stencil X label-dir
		   (label-fret layout props string-count fret-range
			       size orientation)
		   label-space)
		  (ly:stencil-combine-at-edge
		   fret-diagram-stencil Y label-dir
		   (label-fret layout props string-count fret-range
			       size orientation)
		   label-space))))
    (ly:stencil-aligned-to fret-diagram-stencil X alignment)))

(define-builtin-markup-command (fret-diagram layout props definition-string)
  (string?) ; argument type
  fret-diagram ; markup category
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

(define (fret-parse-definition-string props definition-string)
 "Parse a fret diagram string and return a pair containing:
  props, modified as necessary by the definition-string
  a fret-indication list with the appropriate values"
 (let* ((fret-count 4)
	(string-count 6)
	(fret-range (list 1 fret-count))
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

(define-builtin-markup-command
  (fret-diagram-terse layout props definition-string)
  (string?) ; argument type
  fret-diagram ; markup category
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

(define-public 
  (fret-parse-terse-definition-string props definition-string)
  "Parse a fret diagram string that uses terse syntax; return a pair containing:
    props, modified to include the string-count determined by the
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

(define (drop-paren item-list)
  "Drop a final parentheses from a fret indication list
   resulting from a terse string specification of barre."
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
  "Return ALIST-LIST entries for key, in one combined alist.
  There can be two ALIST-LIST entries for a given key. The first
  comes from the override-markup function, the second comes
  from property settings during a regular override.
  This is necessary because some details can be set in one
  place, while others are set in the other.  Both details
  lists must be merged into a single alist.
  Return DEFAULT (optional, else #f) if not
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
