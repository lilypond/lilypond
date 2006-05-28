;;;; stencil.scm -- 
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2003--2006 Han-Wen Nienhuys <hanwen@cs.uu.nl>

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
  (if (null? stils)
      empty-stencil
      (if (null? (cdr stils))
	  (car stils)
	  (ly:stencil-combine-at-edge
	   (car stils) Y dir 
	   (stack-lines dir padding baseline (cdr stils))
	   padding baseline))))

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

(define-public (make-filled-box-stencil xext yext)
  "Make a filled box."
  
  (ly:make-stencil
      (list 'round-filled-box (- (car xext)) (cdr xext)
                       (- (car yext)) (cdr yext) 0.0)
      xext yext))

(define-public (make-circle-stencil radius thickness fill)
  "Make a circle of radius @var{radius} and thickness @var{thickness}"
  (ly:make-stencil
   (list 'circle radius thickness fill) 
   (cons (- radius) radius)
   (cons (- radius) radius)))

(define-public (box-grob-stencil grob)
  "Make a box of exactly the extents of the grob.  The box precisely
encloses the contents.
"
  (let* ((xext (ly:grob-extent grob grob 0))
	 (yext (ly:grob-extent grob grob 1))
	 (thick 0.1))
    
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
  (let* ((x-ext (ly:stencil-extent stencil 0))
	 (y-ext (ly:stencil-extent stencil 1))
	 (diameter (max (- (cdr x-ext) (car x-ext))
			(- (cdr y-ext) (car y-ext))))
	 (radius (+ (/ diameter 2) padding thickness)))
    (ly:stencil-add
     (centered-stencil stencil) (make-circle-stencil radius thickness #f))))

(define-public (fontify-text font-metric text)
  "Set TEXT with font FONT-METRIC, returning a stencil."
  (let* ((b (ly:text-dimension font-metric text)))
    (ly:make-stencil
     `(text ,font-metric ,text) (car b) (cdr b))))
     
(define-public (fontify-text-white scale font-metric text)
  "Set TEXT with scale factor s"
  (let* ((b (ly:text-dimension font-metric text))
	 ;;urg -- workaround for using ps font
         (c `(white-text ,(* 2 scale) ,text)))
    ;;urg -- extent is not from ps font, but we hope it's close
    (ly:make-stencil c (car b) (cdr b))))

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

(define-public (annotate-y-interval layout name extent is-length)
  (let*
      ((text-props (cons
		    '((font-size . -3)
		      (font-family . typewriter))
		    (layout-extract-page-properties layout)))
       (annotation #f)
       )

    ;; do something sensible for 0,0 intervals. 
    (set! extent (interval-widen extent 0.001))
    (if (not (interval-sane? extent))
	(set! annotation (interpret-markup layout text-props
					   (make-simple-markup (format "~a: NaN/inf" name))))
	(let*
	    ((text-stencil (interpret-markup
			    layout text-props
			    (make-column-markup
			     (list
			      (make-whiteout-markup (make-simple-markup name))
			      (make-whiteout-markup
			       (make-simple-markup
				(cond
				 ((interval-empty? extent) "empty")
				 (is-length (format "~$" (interval-length extent)))
				 (else
				  (format "(~$,~$)" (car extent)
					  (cdr extent))))))))))
	     (arrows
	      (ly:stencil-translate-axis 
	       (dimension-arrows (cons 0 (interval-length extent)))
	       (interval-start extent) Y)))
	  
	  (set! annotation
		(ly:stencil-aligned-to text-stencil Y CENTER))
	  
	  (set! annotation (ly:stencil-translate
			    annotation
			    (cons 0 (interval-center extent))))
	  

	  (set! annotation
		(ly:stencil-combine-at-edge arrows X RIGHT annotation 0.5 0))

	  (set! annotation
		(ly:make-stencil (ly:stencil-expr annotation)
				 (ly:stencil-extent annotation X)
				 (cons 10000 -10000)))))
    annotation))


(define-public (eps-file->stencil axis size file-name)
  (let*
      ((contents (ly:gulp-file file-name))
       (bbox (get-postscript-bbox contents))
       (bbox-size (if (= axis X)
		      (- (list-ref bbox 2) (list-ref bbox 0))
		      (- (list-ref bbox 3) (list-ref bbox 1))
		      ))
       (factor (exact->inexact (/ size bbox-size)))
       (scaled-bbox
	(map (lambda (x) (* factor x)) bbox)))

    (if bbox
	(ly:make-stencil
	 (list
	  'embedded-ps
	  (string-append
	   (format
	   "
gsave
currentpoint translate
BeginEPSF
~a ~a scale
%%BeginDocument: ~a
" 	   factor factor
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


(define-public (write-system-signature filename paper-system)
  (define (float? x)
    (and (number? x) (inexact? x)))

  (define system-grob
    (paper-system-system-grob paper-system))
  (define output (open-output-file filename))
  
  (define (strip-floats expr)
    (cond
     ((float? expr) #f)
     ((ly:font-metric? expr) (ly:font-name expr))
     ((pair? expr) (cons (strip-floats (car expr))
			 (strip-floats (cdr expr))))
     (else expr)))

  (define (fold-false-pairs expr)
    (if (pair? expr)
	(let*
	    ((first (car expr))
	     (rest (fold-false-pairs (cdr expr))))

	  (if first
	      (cons (fold-false-pairs first) rest)
	      rest))
	expr))
  
  (define (music-cause grob)
    (let*
	((cause (ly:grob-property  grob 'cause)))
      
      (cond
       ((ly:music? cause) cause)
       ((ly:grob? cause) (music-cause cause))
       (else #f))))

  (define (pythonic-string expr)
    (string-regexp-substitute "'" "\\'" (format "~a" expr)))

  (define (pythonic-pair expr)
    (format "(~a,~a)"
	    (car expr) (cdr expr)))
		    
  (define (found-grob expr)
    (let*
	((grob (car expr))
	 (rest (cdr expr))
	 (collected '())
	 (cause (music-cause grob))
	 (input (if (ly:music? cause) (ly:music-property cause 'origin) #f))
	 (location (if (ly:input-location? input) (ly:input-file-line-char-column input) '()))

	 (x-ext (ly:grob-extent grob system-grob X))
	 (y-ext (ly:grob-extent grob system-grob Y))
	 )

      (interpret-for-signature #f (lambda (e)
				    (set! collected (cons e collected)))
			       rest)

      (format output
	      "['~a', '~a', ~a, ~a, '~a'],\n"
	      (cdr (assq 'name (ly:grob-property grob 'meta) ))
	      (pythonic-string location)
	      (pythonic-pair (if (interval-empty? x-ext) '(0 . 0) x-ext))
	      (pythonic-pair (if (interval-empty? y-ext) '(0 . 0) y-ext))
	      (pythonic-string collected))
      ))

  (define (interpret-for-signature escape collect expr)
    (define (interpret expr)
      (let*
	  ((head (car expr)))

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

  (interpret-for-signature found-grob (lambda (x) #f)
			   (ly:stencil-expr
			    (paper-system-stencil paper-system))))
