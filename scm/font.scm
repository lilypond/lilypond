;; As an excercise, do it with records.
;; Should use GOOPS, really.

;; TODO:
;;
;; lookup-font should be written in  C.
;;
;; should dump tree to .texi as internal documentation
;; 
;; * should extract design sizes from fonts: fonts should 
;; be read off the disk, on demand, something like:
;;  
;; cmr -> ((font-load "cmr6") (font-load "cmr8")  .. )
;;

(define-class <Font-tree-element>
  ()
  )

(define-class <Font-tree-leaf> (<Font-tree-element>)
  (default-size #:init-keyword #:default-size)
  (size-vector  #:init-keyword #:size-vector))

(define-class <Font-tree-node> (<Font-tree-element>)
  (qualifier #:init-keyword #:qualifier  #:accessor font-qualifier)
  (default #:init-keyword #:default #:accessor font-default)
  (children #:init-keyword #:children #:accessor font-children))

(define (make-font-tree-leaf size size-font-vector)
  (make <Font-tree-leaf> #:default-size size #:size-vector size-font-vector))

(define (make-font-tree-node
	 qualifier default)
  (make <Font-tree-node>
     #:qualifier qualifier
     #:default default 
     #:children (make-hash-table 11)))

(define-method (display (leaf <Font-tree-leaf>) port)
  (map (lambda (x) (display x port))
   (list
    "Font-size-family: \n"
     (slot-ref leaf 'default-size) 
     (slot-ref leaf 'size-vector) )))
  
(define-method (display (node <Font-tree-node>) port)
  
    (map
     (lambda (x)
       (display x port))
     
     (list
      "Font_node { \nqual: "
      (font-qualifier node)
      "(def: "
      (font-default node)
      ") {\n"))
    (for-each 
     (lambda (x)
       (display "\n")
       (display (car x) port)
       (display "=" port)
       (display (cdr x) port))
     (hash-table->alist (font-children node)))
    (display "} } \n"))



(define default-qualifier-order
  '(font-encoding font-family font-shape font-series))

(define-method (add-font (node <Font-tree-node>) fprops size-family)
  (define (assoc-delete key alist)
    (assoc-remove! (list-copy alist) key))
  (define (make-node fprops size-family)
    (if (null? fprops)
	(make-font-tree-leaf (car size-family) (cdr size-family))
	(let*
	    ((qual (next-qualifier default-qualifier-order fprops)))
	  (make-font-tree-node qual
			       (assoc-get qual fprops)))
	))
  (define (next-qualifier order props)
    (cond
     ((and (null? props) (null? order))
      #f)
     ((null? props) (car order))
     ((null? order) (caar props))
     (else
      (if (assoc-get (car order) props)
	  (car order)
	  (next-qualifier (cdr order) props))
      )))

  (let*
      ((q (font-qualifier node))
       (d (font-default node))
       (v (assoc-get q fprops d))
       (new-fprops (assoc-delete q fprops))
       (child (hashq-ref (slot-ref node 'children)
			 v #f)))


    (if (not child)
	(begin
	  (set! child (make-node new-fprops size-family))
	  (hashq-set! (slot-ref node 'children) v child)))
    (if (pair? new-fprops)
	(add-font child new-fprops size-family))))


(define-method (add-font (node <Font-tree-leaf>) fprops size-family)
  (throw "must add to node, not leaf"))


(define-method (g-lookup-font (node <Font-tree-node>) alist-chain)
  (let*
	((qual (font-qualifier node))
	 (def (font-default node))
	 (val (chain-assoc-get qual alist-chain def))
	 (desired-font (lookup-font
			(hashq-ref (font-children node)
				   val) alist-chain))

	 (default (hashq-ref (font-children node) def))
	 (font (if desired-font
		   desired-font
		   (g-lookup-font (hashq-ref (font-children node)
					   def) alist-chain)))
	 )
      font))

(define-method (g-lookup-font (node <Font-tree-leaf>) alist-chain)
  node)

(define (lookup-font node alist-chain)
  (g-lookup-font node alist-chain))


(define-public (make-font-tree factor)
  (let*
      ((n (make-font-tree-node 'font-encoding 'music))
       )
    
    (for-each
     (lambda (x)
       (add-font n
		 (list (cons 'font-encoding (car x)))
		 (cons (* factor (cadr x))
		       (caddr x))))
     '((number 10
	       #((3.82  . "feta-nummer4")
		 (5.5  . "feta-nummer6")
		 (8.0  . "feta-nummer8")
		 (10.0  . "feta-nummer10")
		 (12.0  . "feta-nummer12")
		 (16.0  . "feta-nummer16")))
       (dynamic 14.0  #((6.0 . "feta-din6")
			(8.0 . "feta-din8")
			(10.0 . "feta-din10")
			(12.0 . "feta-din12")
			(14.0 . "feta-din14")
			(17.0 . "feta-din17")
			))
       (math 10 #((10.0 . "msam10")))
       (music 20.0
	      #((11.22 . ("feta11" "parmesan11"))
		(12.60 . ("feta13" "parmesan13"))
		(14.14 . ("feta14" "parmesan14"))
		(15.87 . ("feta16" "parmesan16"))
		(17.82 . ("feta18" "parmesan18"))
		(20.0 . ("feta20" "parmesan20"))
		(22.45 . ("feta23" "parmesan23"))
		(25.20 . ("feta26" "parmesan26"))
		))
       (braces 10 #((10.0 . ("feta-braces00"
			     "feta-braces10"
			     "feta-braces20"
			     "feta-braces30"
			     "feta-braces40"
			     "feta-braces50"
			     "feta-braces60"
			     "feta-braces70"
			     "feta-braces80"))
		    ))))

    (for-each
     (lambda (x)
       (add-font
	n
	`((font-encoding . text)
	  (font-series . ,(vector-ref (car x) 2))
	  (font-shape . ,(vector-ref (car x) 1))
	  (font-family . ,(vector-ref (car x) 0)))
	(cons (* factor (cadr x))
	      (cddr x))
	))
     '((#(roman upright medium) .
	(10.0 . #((6.0 . "cmr6")
		  (8.0 . "cmr8") 
		  (10.0 . "cmr10")
		  (17.0 . "cmr17")
		  )))

       (#(roman upright bold) .
	(10.0 . #((6.0 . "cmbx6")
		  (8.0 . "cmbx8")
		  (10.0 . "cmbx10")
		  (12.0 . "cmbx12")
		  )))
       
       (#(roman italic medium) .
	(10.0 . #((7.0 . "cmti7")
		  (10.0 . "cmti10")
		  (12.0 . "cmti12")
		  )))
       (#(roman italic bold) .
	(10.0 . #((8.0 . "cmbxti8")
		  (10.0 . "cmbxti10")
		  (14.0 . "cmbxti14")
		  )))
       
       (#(roman caps medium) .
	(10.0 . #((10.0 . "cmcsc10"))))

       (#(roman upright bold-narrow ) .
	(10.0 . #((10.0 . "cmb10")
		  )))
       
       (#(sans upright medium) .
	(10.0  . #((8.0 . "cmss8")
		   (10.0 . "cmss10")
		   (12.0 . "cmss12")
		   (17.0 . "cmss17")
		   )))
       (#(typewriter upright medium) .
	(10.0 . #((8.0 .  "cmtt8")
		  (10.0 . "cmtt10")
		  (12.0 . "cmtt12")
		  )))
       ))
    n))

; (display (make-font-tree 1.0))

(define-public (magstep x)
  (exp (* (/ x 6) (log 2))))
