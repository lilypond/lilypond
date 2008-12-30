;;;; stencil.scm -- 
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2003--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-public (translate-stencil stencil coordinate-pair)
  "Translate @code{stencil} by the distances specified in
@code{coordinate-pair}."
  (ly:stencil-translate-axis
    (ly:stencil-translate-axis stencil (cdr coordinate-pair) Y)
    (car coordinate-pair) X))

(define-public (stack-stencils axis dir padding stils)
  "Stack stencils STILS in direction AXIS, DIR, using PADDING."
  (cond
   ((null? stils) empty-stencil)
   ((null? (cdr stils)) (car stils))
   (else (ly:stencil-combine-at-edge
	  (car stils) axis dir (stack-stencils axis dir padding (cdr stils))
	  padding))))

(define-public (stack-stencils-padding-list axis dir padding stils)
  "Stack stencils STILS in direction AXIS, DIR, using a list of PADDING."
  (cond
   ((null? stils) empty-stencil)
   ((null? (cdr stils)) (car stils))
   (else (ly:stencil-combine-at-edge
	  (car stils)
	  axis dir
	  (stack-stencils-padding-list axis dir (cdr padding) (cdr stils))
	  (car padding)))))

(define-public (centered-stencil stencil)
  "Center stencil @var{stencil} in both the X and Y directions"
  (ly:stencil-aligned-to (ly:stencil-aligned-to stencil X CENTER) Y CENTER))

(define-public (stack-lines dir padding baseline stils)
  "Stack vertically with a baseline-skip."
  (define result empty-stencil)
  (define last-y #f)
  (do
      ((last-stencil #f (car p))
       (p stils (cdr p)))
      
      ((null? p))

    (if (number? last-y)
	(begin
	  (let* ((dy (max (+ (* dir (interval-bound (ly:stencil-extent last-stencil Y) dir))
			     padding
			     (* (- dir) (interval-bound (ly:stencil-extent (car p) Y) (- dir))))
			  baseline))
		 (y (+ last-y  (* dir dy))))
	    
			  
	    
	    (set! result
		  (ly:stencil-add result (ly:stencil-translate-axis (car p) y Y)))
	    (set! last-y y)))
	(begin
	  (set! last-y 0)
	  (set! result (car p)))))

  result)
    

(define-public (bracketify-stencil stil axis thick protusion padding)
  "Add brackets around STIL, producing a new stencil."

  (let* ((ext (ly:stencil-extent stil axis))
	 (lb (ly:bracket axis ext thick (- protusion)))
	 (rb (ly:bracket axis ext thick protusion)))
    (set! stil
	  (ly:stencil-combine-at-edge stil (other-axis axis) 1 lb padding))
    (set! stil
	  (ly:stencil-combine-at-edge stil (other-axis axis) -1 rb padding))
    stil))

(define-public (make-line-stencil width startx starty endx endy)
  "Make a line stencil of given linewidth and set its extents accordingly"
  (let ((xext (cons (min startx endx) (max startx endx)))
        (yext (cons (min starty endy) (max starty endy))))
    (ly:make-stencil
      (list 'draw-line width startx starty endx endy)
      ; Since the line has rounded edges, we have to / can safely add half the 
      ; width to all coordinates!
      (interval-widen xext (/ width 2))
      (interval-widen yext (/ width 2)))))

(define-public (make-round-filled-box-stencil xext yext blot-diameter)
  "Make a filled rounded box."
  
  (ly:make-stencil
      (list 'round-filled-box (- (car xext)) (cdr xext)
                       (- (car yext)) (cdr yext) blot-diameter)
      xext yext))

(define-public (make-filled-box-stencil xext yext)
  "Make a filled box."
  
  (ly:make-stencil
      (list 'round-filled-box (- (car xext)) (cdr xext)
                       (- (car yext)) (cdr yext) 0.0)
      xext yext))

(define-public (make-circle-stencil radius thickness fill)
  "Make a circle of radius @var{radius} and thickness @var{thickness}"
  (let*
      ((out-radius (+ radius (/ thickness 2.0))))
    
  (ly:make-stencil
   (list 'circle radius thickness fill) 
   (cons (- out-radius) out-radius)
   (cons (- out-radius) out-radius))))

(define-public (make-oval-stencil x-radius y-radius thickness fill)
  "Make an oval from two Bezier curves, of x radius @var{x-radius}, 
    y radius @code{y-radius},
    and thickness @var{thickness} with fill defined by @code{fill}."
  (let*
      ((x-out-radius (+ x-radius (/ thickness 2.0))) 
       (y-out-radius (+ y-radius (/ thickness 2.0))) )
    
  (ly:make-stencil
   (list 'oval x-radius y-radius thickness fill) 
   (cons (- x-out-radius) x-out-radius)
   (cons (- y-out-radius) y-out-radius))))

(define-public (make-ellipse-stencil x-radius y-radius thickness fill)
  "Make an ellipse of x radius @var{x-radius}, y radius @code{y-radius},
    and thickness @var{thickness} with fill defined by @code{fill}."
  (let*
      ((x-out-radius (+ x-radius (/ thickness 2.0))) 
       (y-out-radius (+ y-radius (/ thickness 2.0))) )
    
  (ly:make-stencil
   (list 'ellipse x-radius y-radius thickness fill) 
   (cons (- x-out-radius) x-out-radius)
   (cons (- y-out-radius) y-out-radius))))

(define-public (box-grob-stencil grob)
  "Make a box of exactly the extents of the grob.  The box precisely
encloses the contents.
"
  (let* ((xext (ly:grob-extent grob grob 0))
	 (yext (ly:grob-extent grob grob 1))
	 (thick 0.01))

    (ly:stencil-add
     (make-filled-box-stencil xext (cons (- (car yext) thick) (car yext)))
     (make-filled-box-stencil xext (cons (cdr yext) (+ (cdr yext) thick)))
     (make-filled-box-stencil (cons (cdr xext) (+ (cdr xext) thick)) yext)
     (make-filled-box-stencil (cons (- (car xext) thick) (car xext)) yext))))

;; TODO merge this and prev function. 
(define-public (box-stencil stencil thickness padding)
  "Add a box around STENCIL, producing a new stencil."
  (let* ((x-ext (interval-widen (ly:stencil-extent stencil 0) padding))
	 (y-ext (interval-widen (ly:stencil-extent stencil 1) padding))
	 (y-rule (make-filled-box-stencil (cons 0 thickness) y-ext))
	 (x-rule (make-filled-box-stencil
		  (interval-widen x-ext thickness) (cons 0 thickness))))
    (set! stencil (ly:stencil-combine-at-edge stencil X 1 y-rule padding))
    (set! stencil (ly:stencil-combine-at-edge stencil X -1 y-rule padding))
    (set! stencil (ly:stencil-combine-at-edge stencil Y 1 x-rule 0.0))  
    (set! stencil (ly:stencil-combine-at-edge stencil Y -1 x-rule 0.0))
    stencil))

(define-public (circle-stencil stencil thickness padding)
  "Add a circle around STENCIL, producing a new stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
	 (y-ext (ly:stencil-extent stencil Y))
	 (diameter (max (interval-length x-ext)
                        (interval-length y-ext))) 
	 (radius (+ (/ diameter 2) padding thickness))
	 (circle (make-circle-stencil radius thickness #f)))

    (ly:stencil-add
     stencil
     (ly:stencil-translate circle
			   (cons
			    (interval-center x-ext)
			    (interval-center y-ext))))))

(define-public (oval-stencil stencil thickness x-padding y-padding)
  "Add an oval around @code{stencil}, padded by the padding pair, 
   producing a new stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
	 (y-ext (ly:stencil-extent stencil Y))
         (x-length (+ (interval-length x-ext) x-padding thickness))
         (y-length (+ (interval-length y-ext) y-padding thickness))
         (x-radius (* 0.707 x-length) )
         (y-radius (* 0.707 y-length) )
	 (oval (make-oval-stencil x-radius y-radius thickness #f)))

    (ly:stencil-add
     stencil
     (ly:stencil-translate oval
			   (cons
			    (interval-center x-ext)
			    (interval-center y-ext))))))

(define-public (ellipse-stencil stencil thickness x-padding y-padding)
  "Add an ellipse around STENCIL, padded by the padding pair, 
   producing a new stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
	 (y-ext (ly:stencil-extent stencil Y))
         (x-length (+ (interval-length x-ext) x-padding thickness))
         (y-length (+ (interval-length y-ext) y-padding thickness))
         ;(aspect-ratio (/ x-length y-length))
         (x-radius (* 0.707 x-length) )
         (y-radius (* 0.707 y-length) )
	 ;(diameter (max (- (cdr x-ext) (car x-ext))
	 ;		(- (cdr y-ext) (car y-ext))))
	 ;(radius (+ (/ diameter 2) padding thickness))
	 (ellipse (make-ellipse-stencil x-radius y-radius thickness #f)))

    (ly:stencil-add
     stencil
     (ly:stencil-translate ellipse
			   (cons
			    (interval-center x-ext)
			    (interval-center y-ext))))))

(define-public (rounded-box-stencil stencil thickness padding blot)
   "Add a rounded box around STENCIL, producing a new stencil."  

  (let* ((xext (interval-widen (ly:stencil-extent stencil 0) padding))
	 (yext (interval-widen (ly:stencil-extent stencil 1) padding))
   (min-ext (min (-(cdr xext) (car xext)) (-(cdr yext) (car yext))))
   (ideal-blot (min blot (/ min-ext 2)))
   (ideal-thickness (min thickness (/ min-ext 2)))
	 (outer (ly:round-filled-box
            (interval-widen xext ideal-thickness) 
            (interval-widen yext ideal-thickness) 
               ideal-blot))
	 (inner (ly:make-stencil (list 'color (x11-color 'white) 
            (ly:stencil-expr (ly:round-filled-box 
               xext yext (- ideal-blot ideal-thickness)))))))
    (set! stencil (ly:stencil-add outer inner))
    stencil))


(define-public (fontify-text font-metric text)
  "Set TEXT with font FONT-METRIC, returning a stencil."
  (let* ((b (ly:text-dimension font-metric text)))
    (ly:make-stencil
     `(text ,font-metric ,text) (car b) (cdr b))))
     
(define-public (fontify-text-white scale font-metric text)
  "Set TEXT with scale factor SCALE"
  (let* ((b (ly:text-dimension font-metric text))
	 ;;urg -- workaround for using ps font
         (c `(white-text ,(* 2 scale) ,text)))
    ;;urg -- extent is not from ps font, but we hope it's close
    (ly:make-stencil c (car b) (cdr b))))

(define-public (stencil-with-color stencil color)
  (ly:make-stencil
   (list 'color color (ly:stencil-expr stencil))
   (ly:stencil-extent stencil X)
   (ly:stencil-extent stencil Y)))
  
(define-public (stencil-whiteout stencil)
  (let*
      ((x-ext (ly:stencil-extent stencil X))
       (y-ext (ly:stencil-extent stencil Y))

       )
    
    (ly:stencil-add
     (stencil-with-color (ly:round-filled-box x-ext y-ext 0.0)
			 white)
     stencil)
    ))

(define-public (dimension-arrows destination) 
  "Draw twosided arrow from here to @var{destination}"
  
  (let*
      ((e_x 1+0i)
       (e_y 0+1i)
       (rotate (lambda (z ang)
		 (* (make-polar 1 ang)
		    z)))
       (complex-to-offset (lambda (z)
			    (list (real-part z) (imag-part z))))
       
       (z-dest (+ (* e_x (car destination)) (* e_y (cdr destination))))
       (e_z (/ z-dest (magnitude z-dest)))
       (triangle-points '(-1+0.25i
			  0
			  -1-0.25i))
       (p1s (map (lambda (z)
		   (+ z-dest (rotate z (angle z-dest))))
		 triangle-points))
       (p2s (map (lambda (z)
		   (rotate z (angle (- z-dest))))
		   triangle-points))
       (null (cons 0 0)) 
       (arrow-1  
	(ly:make-stencil
	 `(polygon (quote ,(concatenate (map complex-to-offset p1s)))
		   0.0
		   #t) null null))
       (arrow-2
	(ly:make-stencil
	 `(polygon (quote ,(concatenate (map complex-to-offset p2s)))
		   0.0
		   #t) null null ) )
       (thickness 0.1)
       (shorten-line 0.5)
       (start (complex-to-offset (/ (* e_z shorten-line) 2)))
       (end (complex-to-offset (- z-dest (/ (* e_z shorten-line) 2))))
       
       (line (ly:make-stencil
	      `(draw-line ,thickness
			  ,(car start) ,(cadr start)
			  ,(car end) ,(cadr end)
			  )
	      (cons (min 0 (car destination))
		    (min 0 (cdr destination)))
	      (cons (max 0 (car destination))
		    (max 0 (cdr destination)))))
		    
       (result (ly:stencil-add arrow-2 arrow-1 line)))


    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANNOTATIONS
;;
;; annotations are arrows indicating the numerical value of
;; spacing variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define*-public (annotate-y-interval layout name extent is-length
                                     #:key (color darkblue))
  (let ((text-props (cons '((font-size . -3)
			    (font-family . typewriter))
			  (layout-extract-page-properties layout)))
	(annotation #f))
    (define (center-stencil-on-extent stil)
      (ly:stencil-translate (ly:stencil-aligned-to stil Y CENTER)
                            (cons 0 (interval-center extent))))
    ;; do something sensible for 0,0 intervals. 
    (set! extent (interval-widen extent 0.001))
    (if (not (interval-sane? extent))
	(set! annotation (interpret-markup
			  layout text-props
			  (make-simple-markup (simple-format #f "~a: NaN/inf" name))))
	(let ((text-stencil (interpret-markup
			     layout text-props
                             (markup #:whiteout #:simple name)))
              (dim-stencil (interpret-markup
                            layout text-props
                            (markup #:whiteout
                                    #:simple (cond
                                              ((interval-empty? extent)
                                               (format "empty"))
                                              (is-length
                                               (ly:format "~$" (interval-length extent)))
                                              (else
                                               (ly:format "(~$,~$)"
                                                       (car extent) (cdr extent)))))))
	      (arrows (ly:stencil-translate-axis 
		       (dimension-arrows (cons 0 (interval-length extent)))
		       (interval-start extent) Y)))
	  (set! annotation
                (center-stencil-on-extent text-stencil))
	  (set! annotation
		(ly:stencil-combine-at-edge arrows X RIGHT annotation 0.5))
	  (set! annotation
		(ly:stencil-combine-at-edge annotation X LEFT
                                            (center-stencil-on-extent dim-stencil)
                                            0.5))
	  (set! annotation
		(ly:make-stencil (list 'color color (ly:stencil-expr annotation))
				 (ly:stencil-extent annotation X)
				 (cons 10000 -10000)))))
    annotation))


(define-public (eps-file->stencil axis size file-name)
  (let*
      ((contents (ly:gulp-file file-name))
       (bbox (get-postscript-bbox (car (string-split contents #\nul))))
       (bbox-size (if (= axis X)
		      (- (list-ref bbox 2) (list-ref bbox 0))
		      (- (list-ref bbox 3) (list-ref bbox 1))
		      ))
       (factor (if (< 0 bbox-size)
		   (exact->inexact (/ size bbox-size))
		   0))
       (scaled-bbox
	(map (lambda (x) (* factor x)) bbox))
       (clip-rect-string (ly:format
			  "~a ~a ~a ~a rectclip"
			  (list-ref bbox 0) 
			  (list-ref bbox 1) 
			  (- (list-ref bbox 2) (list-ref bbox 0))
			  (- (list-ref bbox 3) (list-ref bbox 1)))))
    

    (if bbox
	(ly:make-stencil
	 (list
	  'embedded-ps
	  (string-append
	   (ly:format
	   "
gsave
currentpoint translate
BeginEPSF
~a dup scale
~a 
%%BeginDocument: ~a
" 	   factor clip-rect-string

	   file-name
	   )
	   contents
	   "%%EndDocument
EndEPSF
grestore
"))
	
	 (cons (list-ref scaled-bbox 0) (list-ref scaled-bbox 2))
	 (cons (list-ref scaled-bbox 1) (list-ref scaled-bbox 3)))
	
	(ly:make-stencil "" '(0 . 0) '(0 . 0)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output signatures.

(define-public (write-system-signatures basename paper-systems count)
  (if (pair? paper-systems)
      (begin
	(let*
	    ((outname (simple-format #f "~a-~a.signature" basename count)) )
	     
	  (ly:message "Writing ~a" outname)
	  (write-system-signature outname (car paper-systems))
	  (write-system-signatures basename (cdr paper-systems) (1+ count))))))

(use-modules (scm paper-system))
(define-public (write-system-signature filename paper-system)
  (define (float? x)
    (and (number? x) (inexact? x)))

  (define system-grob
    (paper-system-system-grob paper-system))
  
  (define output (open-output-file filename))

  ;; todo: optionally use a command line flag? Or just junk this?
  (define compare-expressions #f)
  (define (strip-floats expr)
    "Replace floats by #f"
    (cond
     ((float? expr) #f)
     ((ly:font-metric? expr) (ly:font-name expr))
     ((pair? expr) (cons (strip-floats (car expr))
			 (strip-floats (cdr expr))))
     (else expr)))

  (define (fold-false-pairs expr)
    "Try to remove lists of #f as much as possible."
    (if (pair? expr)
	(let*
	    ((first (car expr))
	     (rest (fold-false-pairs (cdr expr))))

	  (if first
	      (cons (fold-false-pairs first) rest)
	      rest))
	expr))
  
  (define (raw-string expr)
    "escape quotes and slashes for python consumption"
    (regexp-substitute/global #f "[@\n]" (simple-format #f "~a" expr) 'pre " " 'post))

  (define (raw-pair expr)
    (simple-format #f "~a ~a"
	    (car expr) (cdr expr)))
  
  (define (found-grob expr)
    (let*
	((grob (car expr))
	 (rest (cdr expr))
	 (collected '())
	 (cause (event-cause grob))
	 (input (if (ly:stream-event? cause) (ly:event-property cause 'origin) #f))
	 (location (if (ly:input-location? input) (ly:input-file-line-char-column input) '()))

	 ;; todo: use stencil extent if available.
	 (x-ext (ly:grob-extent grob system-grob X))
	 (y-ext (ly:grob-extent grob system-grob Y))
	 (expression-skeleton
	  (if compare-expressions
	      (interpret-for-signature
	       #f (lambda (e)
		    (set! collected (cons e collected)))
	       rest)
	     "")))

      (simple-format output
	      "~a@~a@~a@~a@~a\n"
	      (cdr (assq 'name (ly:grob-property grob 'meta) ))
	      (raw-string location)
	      (raw-pair (if (interval-empty? x-ext) '(1 . -1) x-ext))
	      (raw-pair (if (interval-empty? y-ext) '(1 . -1) y-ext))
	      (raw-string collected))
      ))

  (define (interpret-for-signature escape collect expr)
    (define (interpret expr)
      (let*
	  ((head (if (pair? expr)
		     (car expr)
		     #f)))

	(cond
	 ((eq? head 'grob-cause) (escape (cdr expr)))
	 ((eq? head 'color) (interpret (caddr expr)))
	 ((eq? head 'rotate-stencil) (interpret (caddr expr)))
	 ((eq? head 'translate-stencil) (interpret (caddr expr)))
	 ((eq? head 'combine-stencil)
	  (for-each (lambda (e) (interpret e))  (cdr expr)))
	 (else
	  (collect (fold-false-pairs (strip-floats expr))))
	 
	 )))

    (interpret expr))

  (if (ly:grob? system-grob)
      (begin
	(display (simple-format #f "# Output signature\n# Generated by LilyPond ~a\n" (lilypond-version))
		 output)
	(interpret-for-signature found-grob (lambda (x) #f)
				 (ly:stencil-expr
				  (paper-system-stencil paper-system)))))

  ;; should be superfluous, but leaking "too many open files"?
  (close-port output))
  
