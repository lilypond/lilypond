


(define-public (simple-markup grob props . rest)
  (Text_item::text_to_molecule grob props (car rest))
  )

(define-public (line-markup grob props . rest)
  (stack-molecules
   X 1 1.0 
   (map (lambda (x) (interpret-markup grob props x)) (car rest)))
  )

(define (combine-molecule-list lst)
  (if (null? (cdr lst)) (car lst)
      (ly:add-molecule (car lst) (combine-molecule-list (cdr lst)))
      ))

(define-public (combine-markup grob props . rest)
   (combine-molecule-list (map (lambda (x) (interpret-markup grob props x)) (car rest))))

(define (font-markup qualifier value)
  (lambda (grob props . rest)
    (interpret-markup grob (cons (cons `(,qualifier . ,value) (car props)) (cdr props)) (car rest))
  
  ))

(define-public bold-markup
  (font-markup 'font-series 'bold))
(define-public dynamic-markup
  (font-markup 'font-family 'dynamic))
(define-public italic-markup
  (font-markup 'font-shape 'italic))

(define-public (column-markup grob props . rest)
  (stack-lines
   -1 0.0 (cdr (chain-assoc 'baseline-skip props))
   (map (lambda (x) (interpret-markup grob props x)) (car rest)))
  )

(define-public (music-markup grob props . rest)
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
  (ly:get-glyph  (ly:get-font grob props) (car rest))
		 
  )
(define-public (raise-markup grob props  . rest)
  (ly:molecule-translate-axis (interpret-markup grob props (cadr rest))
			      (car rest) Y)
  )

;; this is too simplistic: doesn't do backup for negative dimensions.
(define (hspace-markup grob props . rest)
  (ly:make-molecule "" (cons 0 (car rest)) '(-1 . 1) )
  )

(define-public (override-markup grob props . rest)
  "Tack the 1st args in REST onto PROPS."
  (interpret-markup grob (cons (list (car rest)) props)
		    (cadr rest)))

(map (lambda (x)
       (set-object-property! (car x) 'markup-signature (cdr x))
       )
     (list (cons bold-markup 'markup0)
	   (cons column-markup 'markup-list0)
	   (cons line-markup  'markup-list0)
	   (cons combine-markup 'markup0-markup1)
	   (cons simple-markup 'markup0)
	   (cons music-markup 'scm0)
	   (cons override-markup 'scm0-markup1)
	   (cons lookup-markup 'scm0)
	   (cons raise-markup 'scm0-markup1)
	   (cons italic-markup 'markup0)
	   (cons dynamic-markup 'markup0)
	   (cons char-markup 'scm0)
	   (cons hspace-markup 'scm0)
	   
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
