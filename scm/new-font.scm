;; As an excercise, do it with records.
;; Should use GOOPS, really.

;; TODO:
;;
;; lookup-font should be written in  C.
;;
;; should dump tree to .texi as internal documentation
;; 


(define font-tree-record
  (make-record-type
   "font-tree-node"
   '(qualifier default children)))

(define-public font-tree-node?
  (record-predicate font-tree-record))
(define-public font-tree-default
  (record-accessor font-tree-record 'default))
(define-public font-tree-qualifier
  (record-accessor font-tree-record 'qualifier))
(define-public font-tree-children
  (record-accessor font-tree-record 'children))


(define (make-font-tree-node
	 qualifier default)
  ((record-constructor font-tree-record)
    qualifier
    default
    (make-hash-table 11)))		;ugh. hardcoded.

(define default-qualifier-order
  '(font-encoding font-family font-shape font-series))


(define-public (add-font node fprops size-family)
  (define (assoc-delete key alist)
    (assoc-remove! (list-copy alist) key))
  (define (make-node fprops size-family)
    (if (null? fprops)
	size-family
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

  (if (font-tree-node? node)
      (let*
	  ((q (font-tree-qualifier node))
	   (d (font-tree-default node))
	   (v (assoc-get q fprops d))
	   (new-fprops (assoc-delete q fprops))
	   (child (hashq-ref (font-tree-children node)
			     v #f)))


	(if (not child)
	    (begin
	      (set! child (make-node new-fprops size-family))
	      (hashq-set! (font-tree-children node) v child)))

	(add-font child new-fprops size-family))
      (if (not (equal? size-family node))
	  (throw 'invalid-font props size-family)))
  )

(define-public (display-font-node node . rest)
  (let*
      ((port (if (pair? rest) (car rest) (current-output-port)))
       )
    (cond
     ((font-tree-node? node)
      (map
       (lambda (x)
	 (display x port))
       
       (list
	"Font_node { \nqual: "
	(font-tree-qualifier node)
	"(def: "
	(font-tree-default node)
	") {\n"))
      (for-each 
       (lambda (x)
	 (display "\n")
	 (display (car x) port)
	 (display "=" port)
	 (display-font-node (cdr x) port))
       (hash-table->alist (font-tree-children node)))
      (display "} } \n"))

     (else
      (display node port))))
  )

(define-public (lookup-font node alist-chain)
  (cond
   ((font-tree-node? node)
    (let*
	((qual (font-tree-qualifier node))
	 (def (font-tree-default node))
	 (val (chain-assoc-get qual alist-chain def))
	 (desired-font (lookup-font
			(hashq-ref (font-tree-children node)
				   val) alist-chain))
	 (font (if desired-font
		   desired-font
		   (lookup-font (hashq-ref (font-tree-children node)
					   def) alist-chain)))
	 )
      font))
   (else node))
   )

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
     '((number 10 #((4.0  . "feta-nummer4")
		    (6.0  . "feta-nummer6")
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

(define-public (magstep x)
  (exp (* (/ x 6) (log 2))))
