(define-public (simple-markup grob props . rest)
  (Text_item::text_to_molecule grob props (car rest))
  )

(define-public (stack-molecule-line space molecules)
  (if (pair? molecules)
      (if (pair? (cdr molecules))
	  (let* (
		 (tail (stack-molecule-line  space (cdr molecules)))
		 (head (car molecules))
		 (xoff (+ space (cdr (ly:get-molecule-extent head X))))
		 )
	    
	    (ly:add-molecule
	     head
	     (ly:molecule-translate-axis tail xoff X))
	  )
	  (car molecules))
      '())
  )

(define-public (line-markup grob props . rest)
  (stack-molecule-line
   (cdr (chain-assoc 'word-space props))
   (map (lambda (x) (interpret-markup grob props x)) (car rest)))
  )

(define (combine-molecule-list lst)
  (if (null? (cdr lst)) (car lst)
      (ly:add-molecule (car lst) (combine-molecule-list (cdr lst)))
      ))

(define-public (combine-markup grob props . rest)
  (ly:add-molecule
   (interpret-markup grob props (car rest))
   (interpret-markup grob props (cadr rest))))
  
;   (combine-molecule-list (map (lambda (x) (interpret-markup grob props x)) (car rest))))

(define (font-markup qualifier value)
  (lambda (grob props . rest)
    (interpret-markup grob (cons (cons `(,qualifier . ,value) (car props)) (cdr props)) (car rest))
  
  ))


(define-public (set-property-markup qualifier)
  (lambda (grob props . rest  )
    (interpret-markup grob
		      (cons (cons `(,qualifier . ,(car rest))
				  (car props)) (cdr props))
		      (cadr rest))
    ))


(define-public fontsize-markup (set-property-markup 'font-relative-size))
(define-public magnify-markup (set-property-markup 'font-magnification))

(define-public bold-markup
  (font-markup 'font-series 'bold))
(define-public number-markup
  (font-markup 'font-family 'number))


(define-public huge-markup
  (font-markup 'font-relative-size 2))
(define-public large-markup
  (font-markup 'font-relative-size 1))
(define-public small-markup
  (font-markup 'font-relative-size -1))
(define-public tiny-markup
  (font-markup 'font-relative-size -2))
(define-public teeny-markup
  (font-markup 'font-relative-size -3))
(define-public dynamic-markup
  (font-markup 'font-family 'dynamic))
(define-public italic-markup
  (font-markup 'font-shape 'italic))


;; TODO: baseline-skip should come from the font.
(define-public (column-markup grob props . rest)
  (stack-lines
   -1 0.0 (cdr (chain-assoc 'baseline-skip props))
   (map (lambda (x) (interpret-markup grob props x)) (car rest)))
  )

(define-public (musicglyph-markup grob props . rest)
  (ly:find-glyph-by-name
   (ly:get-font grob (cons '((font-family . music)) props))
   (car rest))
  )

(define-public (lookup-markup grob props . rest)
  "Lookup a glyph by name."
  (ly:find-glyph-by-name
   (ly:get-font grob props)
   (car rest))
  )

(define-public (char-markup grob props . rest)
  "Syntax: \\char NUMBER. "
  (ly:get-glyph  (ly:get-font grob props) (car rest))
		 
  )
(define-public (raise-markup grob props  . rest)
  "Syntax: \\raise AMOUNT MARKUP. "
  (ly:molecule-translate-axis (interpret-markup grob props (cadr rest))
			      (car rest) Y)
  )

(define-public (super-markup grob props  . rest)
  "Syntax: \\super MARKUP. "
  (ly:molecule-translate-axis (interpret-markup grob props (car rest))
			      (* 0.5 (cdr (chain-assoc 'baseline-skip props)))
			      Y)
  )

(define-public (translate-markup grob props . rest)
  "Syntax: \\translate OFFSET MARKUP. "
  (ly:molecule-translate (interpret-markup  grob props (cadr rest))
			 (car rest))

  )

(define-public (sub-markup grob props  . rest)
  "Syntax: \\sub MARKUP."
  (ly:molecule-translate-axis (interpret-markup grob props (car rest))
			      (* -0.5 (cdr (chain-assoc 'baseline-skip props)))
			      Y)
  )

;; todo: fix negative space
(define (hspace-markup grob props . rest)
  "Syntax: \\hspace NUMBER."
  (let*
      ((amount (car rest)))
    (if (> amount 0)
	(ly:make-molecule "" (cons 0 amount) '(-1 . 1) )
	(ly:make-molecule "" (cons amount amount) '(-1 . 1)))
  ))

(define-public (override-markup grob props . rest)
  "Tack the 1st args in REST onto PROPS."
  (interpret-markup grob (cons (list (car rest)) props)
		    (cadr rest)))

(map (lambda (x)
       (set-object-property! (car x) 'markup-signature (cdr x))
       )
     (list
      (cons bold-markup 'markup0)
      (cons teeny-markup 'markup0)
      (cons tiny-markup 'markup0)
      (cons small-markup 'markup0)
      (cons italic-markup 'markup0)
      (cons dynamic-markup 'markup0)
      (cons large-markup 'markup0) 
      (cons huge-markup 'markup0) 
      (cons sub-markup 'markup0)
      (cons super-markup 'markup0)
      (cons number-markup 'markup0)

      (cons column-markup 'markup-list0)
      (cons line-markup  'markup-list0)

      (cons combine-markup 'markup0-markup1)

      (cons simple-markup 'markup0)
      (cons musicglyph-markup 'scm0)
      (cons translate-markup 'scm0-markup1)
      (cons override-markup 'scm0-markup1)
      (cons lookup-markup 'scm0)
      (cons raise-markup 'scm0-markup1)
      (cons char-markup 'scm0)
      (cons hspace-markup 'scm0)
      (cons magnify-markup 'scm0-markup1)
      (cons fontsize-markup 'scm0-markup1)
      (cons translate-markup 'scm0-markup1)
      ))


(define markup-module (current-module))

(define-public (lookup-markup-command code)
  (let*
      ( (sym (string->symbol (string-append code "-markup")))
	(var (module-local-variable markup-module sym))
	)
    (if (eq? var #f)
	#f   
	(cons (variable-ref var) (object-property  (variable-ref var) 'markup-signature))
    )
  ))


(define-public (brew-new-markup-molecule grob)
  (interpret-markup grob
		    (Font_interface::get_property_alist_chain grob)
		    (ly:get-grob-property grob 'text)
		    )
  )

(define (interpret-markup  grob props markup)
  (let*
      (
       (func (car markup))
       (args (cdr markup))
       )
    
    (apply func (cons grob (cons props args)) )
    ))


(define (new-markup? x)
	(markup-function? (car x))
)

(define (markup-function? x)
	(object-property 'markup-signature? x))
