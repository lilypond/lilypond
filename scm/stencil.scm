;;;; stencil.scm -- 
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2003--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-public (stack-stencils axis dir padding stils)
  "Stack stencils STILS in direction AXIS, DIR, using PADDING."
  (if (null? stils)
      '()
      (if (pair? stils)
	  (ly:stencil-combine-at-edge
	   (car stils) axis dir (stack-stencils axis dir padding (cdr stils))
	   padding))))

(define-public (stack-lines dir padding baseline stils)
  "Stack vertically with a baseline-skip."
  (if (null? stils)
      '()
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
      (list 'filledbox (- (car xext)) (cdr xext)
                       (- (car yext)) (cdr yext))
      xext yext))


(define-public (box-grob-stencil grob)
  "Make a box of exactly the extents of the grob.  The box precisely
encloses the contents.
"
  (let* ((xext (ly:grob-extent grob grob 0))
	 (yext (ly:grob-extent grob grob 1))
	 (thick 0.1))

    (ly:stencil-add
     (make-filled-box-stencil xext (cons (- (car yext) thick) (car yext)))
     (make-filled-box-stencil xext (cons  (cdr yext) (+ (cdr yext) thick)))
     (make-filled-box-stencil (cons (cdr xext) (+ (cdr xext) thick)) yext)
     (make-filled-box-stencil (cons (- (car xext) thick) (car xext)) yext))))

;; TODO merge this and prev function. 
(define-public (box-stencil stil thick padding)
  "Add a box around STIL, producing a new stencil."
  (let* ((x-ext (interval-widen (ly:stencil-extent stil 0) padding))
	 (y-ext (interval-widen (ly:stencil-extent stil 1) padding))
	 (y-rule (make-filled-box-stencil (cons 0 thick) y-ext))
	 (x-rule (make-filled-box-stencil (interval-widen x-ext thick)
					   (cons 0 thick))))
    
    (set! stil (ly:stencil-combine-at-edge stil X 1 y-rule padding))
    (set! stil (ly:stencil-combine-at-edge stil X -1 y-rule padding))
    (set! stil (ly:stencil-combine-at-edge stil Y 1 x-rule 0.0))  
    (set! stil (ly:stencil-combine-at-edge stil Y -1 x-rule 0.0))
    
    stil))

(define-public (fontify-text font-metric text)
  "Set TEXT with font FONT-METRIC, returning a stencil."
  (let* ((b  (ly:text-dimension font-metric text)))
    (ly:make-stencil
     `(text ,font-metric ,text) (car b) (cdr b))))
     
(define-public (fontify-text-white scale font-metric text)
  "Set TEXT with scale factor s"
  (let* ((b  (ly:text-dimension font-metric text))
         (c  `(white-text ,(* 2 scale) ,text))) ;urg -- workaround for using ps font
    (ly:make-stencil c  (car b) (cdr b))))  ;urg -- extent is not from ps font, but we hope it's close
