

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)
  ))))


;;; TODO

(define (wordwrap string)
  ""
  )
  

(define (self-evaluating? x)
  (or (number? x) (string? x))
  )
      
(define (type-name  predicate)
  (cond
   ((eq? predicate dir?) "direction")
   ((eq? predicate ly-element?) "graphic element")
   ((eq? predicate pair?) "pair")
   ((eq? predicate integer?) "integer")
   ((eq? predicate list?) "list")
   ((eq? predicate symbol?) "symbol")
   ((eq? predicate string?) "string")
   ((eq? predicate boolean?) "string")   
   ((eq? predicate number?) "number")
   ((eq? predicate procedure?) "procedure") 
   (else "(unknown)")
  ))

(define (scm->string val)
  (string-append
   (if (self-evaluating? val) "" "'")
   (call-with-output-string (lambda (port) (display val port)))
  ))

(define (document-property prop desc)
  (let ((handle (assoc (car prop) desc)))
    (string-append
     "\n" (symbol->string (car prop)) " (" (type-name (cadr prop)) ") -- "
     (caddr prop)
     "\ndefault value:  "
     (if (pair? handle)
	 (scm->string (cdr handle))
	 "not set"
	 )
     "\n"
  )
  ))

;;
;; todo: setup ifaces differently.
;;
(define (document-element description)
  (let* ((metah (assoc 'meta description))
	 (meta (if (pair? metah)
		   (cdr metah)
		   '((properties . ()) (name . "huh?"))
		   ))
	 
	 (name (cdr (assoc 'name meta)))
;	 (iface-descs (cdr (assoc 'interface-descriptions meta)))
	 (propdesc (cdr (assoc 'properties meta)))
	 (docs (map (lambda (x) (document-property x description))
		    (uniqued-alist propdesc '())))
	 )

    (string-append
     "\n-------------------------\n"
     name "\n"
     "-------------------------\n"
     "INTERFACES\n"
     "(todo)\n"
     ; (apply string-append iface-descs)
     "-------------------------\n"
     "PROPERTIES:\n"
     (apply string-append docs)
    )
  ))


(define (document-elements elts)
  (string-append
   (map (lambda (x) (display (car x)) (document-element (cdr x)))
	elts
	)
   ))

(define (test-module )
  (display
   (list
  (scm->string '(1 2 abc))
  (scm->string +)
  (type-name number?)
  (uniqued-alist '((a . 1 ) (a . 1)) '() )
  )))


; (define b (cdr (assoc 'Beam all-element-descriptions)))
;(display b)

; (display (document-element  b))

(display (document-elements all-element-descriptions))

