


(define-public (simple-markup grob props . rest)
  (Text_item::text_to_molecule grob props (car rest))
  )

(define-public (line-markup grob props . rest)
  (stack-molecules
   X 1 1.0 
   (map (lambda (x) (interpret_markup grob props x)) (car rest)))
  )

(define (combine-molecule-list lst)
  (if (null? (cdr lst)) (car lst)
      (ly:add-molecule (car lst) (combine-molecule-list (cdr lst)))
      ))

(define-public (combine-markup grob props . rest)
   (combine-molecule-list (map (lambda (x) (interpret_markup grob props x)) (car rest))))

(define-public (bold-markup grob props . rest)
   (interpret_markup grob (cons '((font-series .  bold)) props) (car rest))
  )

(define-public (column-markup grob props . rest)
  (stack-molecules
   Y -1 0.0 
   (map (lambda (x) (interpret_markup grob props x)) (car rest)))
  )

;; todo. Use macro?  
(map
 (lambda (x)

   (set-object-property!
    (eval (string->symbol (string-append (symbol->string x) "-markup")) (current-module))
    'markup-function? #t))

 '(simple column bold combine line )
 )

(define-public (brew-new-markup-molecule grob)
  (interpret_markup grob
		    (Font_interface::get_property_alist_chain grob)
		    (ly:get-grob-property grob 'text)
		    )
  )

(define (interpret_markup  grob props markup)
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
	(object-property 'markup-function? x))
