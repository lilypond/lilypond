;;;; font.scm -- construct font trees
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

;; TODO:
;;
;; lookup-font should be written in  C.
;;

(define-class <Font-tree-element>
  ())

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
	"#<Font-size-family:\n"
	(slot-ref leaf 'default-size) 
	(slot-ref leaf 'size-vector)
	"#>"
	)))

(define-method (display (node <Font-tree-node>) port)

  (map
   (lambda (x)
     (display x port))

   (list
    "Font_node {\nqual: "
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
  (display "} }\n"))


(define default-qualifier-order
  '(font-encoding font-family font-shape font-series))

(define-method (add-font (node <Font-tree-node>) fprops size-family)
  (define (assoc-delete key alist)
    (assoc-remove! (list-copy alist) key))
  
  (define (make-node fprops size-family)
    (if (null? fprops)
	(make-font-tree-leaf (car size-family) (cdr size-family))
	(let* ((qual (next-qualifier default-qualifier-order fprops)))
	  (make-font-tree-node qual
			       (assoc-get qual fprops)))))

  (define (next-qualifier order props)
    (cond
     ((and (null? props) (null? order))
      #f)
     ((null? props) (car order))
     ((null? order) (caar props))
     (else
      (if (assoc-get (car order) props)
	  (car order)
	  (next-qualifier (cdr order) props)))))

  (let* ((q (font-qualifier node))
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
  (let* ((qual (font-qualifier node))
	 (def (font-default node))
	 (val (chain-assoc-get qual alist-chain def))
	 (desired-child (hashq-ref (font-children node) val)))

    (if desired-child
	(g-lookup-font desired-child alist-chain)
	(g-lookup-font (hashq-ref (font-children node) def) alist-chain))))


(define-method (g-lookup-font (node <Font-tree-leaf>) alist-chain)
  node)

;; two step call  is handy for debugging.
(define (lookup-font node alist-chain)
  (g-lookup-font node alist-chain))

;; Each size family is a vector of fonts, loaded with a delay.  The
;; vector should be sorted according to ascending design size.
(define feta-alphabet-size-vector
  (if (defined? 'ly:kpathsea-find-file)
      `#(,(delay  (ly:font-load "feta-alphabet11"))
	 ,(delay  (ly:font-load "feta-alphabet13"))
	 ,(delay  (ly:font-load "feta-alphabet14"))
	 ,(delay  (ly:font-load "feta-alphabet16"))
	 ,(delay  (ly:font-load "feta-alphabet18"))
	 ,(delay  (ly:font-load "feta-alphabet20"))
	 ,(delay  (ly:font-load "feta-alphabet23"))
	 ,(delay  (ly:font-load "feta-alphabet26")))
      (list->vector
       (map (lambda (tup)
	      (cons (ly:pt (cdr tup))
		    (format "feta-alphabet~a ~a"
			    (car tup)
			    (ly:pt (cdr tup)))))
	    '((11 . 11.22)
	      (13 . 12.60)
	      (14 .  14.14)
	      (16 . 15.87)
	      (18 . 17.82)
	      (20 . 20)
	      (23 . 22.45)
	      (26 . 25.20))))))

(define-public (add-music-fonts node factor)
  (for-each
   (lambda (x)
     (add-font node
	       (list (cons 'font-encoding (car x)))
	       (cons (* factor (cadr x))
		     (caddr x))))
   `((fetaDynamic ,(ly:pt 20.0)  ,feta-alphabet-size-vector)
     (fetaNumber ,(ly:pt 20.0)  ,feta-alphabet-size-vector)
     (fetaMusic ,(ly:pt 20.0)
		#(,(delay  (ly:font-load "emmentaler-11"))
		  ,(delay  (ly:font-load "emmentaler-13"))		  
		  ,(delay  (ly:font-load "emmentaler-14"))
		  ,(delay  (ly:font-load "emmentaler-16"))		  
		  ,(delay  (ly:font-load "emmentaler-18"))
		  ,(delay  (ly:font-load "emmentaler-20"))		  
		  ,(delay  (ly:font-load "emmentaler-23"))		  
		  ,(delay  (ly:font-load "emmentaler-26"))))

     (fetaBraces ,(ly:pt 20.0) #(,(delay
			    (ly:font-load "aybabtu")))))))

(define-public (add-cmr-fonts node factor)
  (add-font node '((font-encoding . TeX-math))
	    `(,(* factor 10) . #(,(delay (ly:font-load "msam10")))))
  (for-each
   (lambda (x)
     (add-font node `((font-encoding . TeX-text)
		      (font-series . ,(vector-ref (car x) 2))
		      (font-shape . ,(vector-ref (car x) 1))
		      (font-family . ,(vector-ref (car x) 0)))
	       (cons (* factor (cadr x))
		     (cddr x))))
   `((#(roman upright medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "cmr6"))
		  ,(delay (ly:font-load "cmr8")) 
		  ,(delay (ly:font-load "cmr10"))
		  ,(delay (ly:font-load "cmr17")))))
     (#(roman upright bold) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "cmbx6"))
		  ,(delay (ly:font-load "cmbx8"))
		  ,(delay (ly:font-load "cmbx10"))
		  ,(delay (ly:font-load "cmbx12")))))
     (#(roman italic medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "cmti7"))
		  ,(delay (ly:font-load "cmti10"))
		  ,(delay (ly:font-load "cmti12")))))
     (#(roman italic bold) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "cmbxti8"))
		  ,(delay (ly:font-load "cmbxti10"))
		  ,(delay (ly:font-load "cmbxti14")))))
     (#(roman caps medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "cmcsc10")))))
     (#(roman upright bold-narrow ) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "cmb10")))))
     (#(sans upright medium) 
      . (,(ly:pt 10.0)  . #(,(delay (ly:font-load "cmss8"))
		   ,(delay (ly:font-load "cmss10"))
		   ,(delay (ly:font-load "cmss12"))
		   ,(delay (ly:font-load "cmss17")))))
     (#(typewriter upright medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "cmtt8"))
		  ,(delay (ly:font-load "cmtt10"))
		  ,(delay (ly:font-load "cmtt12"))))))))

;; Debian lmodern font support.
(define-public (add-cork-lm-fonts node factor)
  (for-each
   (lambda (x)
     (add-font node `((font-encoding . cork-lm)
		      (font-series . ,(vector-ref (car x) 2))
		      (font-shape . ,(vector-ref (car x) 1))
		      (font-family . ,(vector-ref (car x) 0)))
	       (cons (* factor (cadr x)) (cddr x))))
   `((#(roman upright medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "lmr6"))
		  ,(delay (ly:font-load "lmr8"))
		  ,(delay (ly:font-load "lmr10"))
		  ,(delay (ly:font-load "lmr17")))))
     (#(roman upright bold) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "lmbx6"))
		  ,(delay (ly:font-load "lmbx8"))
		  ,(delay (ly:font-load "lmbx10"))
		  ,(delay (ly:font-load "lmbx12")))))
     (#(roman italic medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "lmri7"))
		  ,(delay (ly:font-load "lmri10"))
		  ,(delay (ly:font-load "lmri12")))))
     (#(roman italic bold)
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "lmbxi10")))))
     (#(roman caps medium)
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "lmcsc10")))))
     (#(roman upright bold-narrow ) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "lmb10")))))
     (#(sans upright medium) 
      . (,(ly:pt 10.0)  . #(,(delay (ly:font-load "lmss8"))
		   ,(delay (ly:font-load "lmss10"))
		   ,(delay (ly:font-load "lmss12"))
		   ,(delay (ly:font-load "lmss17")))))
     (#(sans upright bold) 
      . (,(ly:pt 10.0)  . #(,(delay (ly:font-load "lmssbx10")))))

     (#(typewriter upright medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "lmtt8"))
		  ,(delay (ly:font-load "lmtt10"))
		  ,(delay (ly:font-load "lmtt12"))))))))

;; ec-fonts-mftraced font support.
(define-public (add-ec-fonts node factor)
  (for-each
   (lambda (x) (add-font node
			 `((font-encoding . Extended-TeX-Font-Encoding---Latin)
			   (font-series . ,(vector-ref (car x) 2))
			   (font-shape . ,(vector-ref (car x) 1))
			   (font-family . ,(vector-ref (car x) 0)))
			 (cons (* factor (cadr x)) (cddr x))))

   `((#(roman upright medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "ecrm6"))
		  ,(delay (ly:font-load "ecrm8"))
		  ,(delay (ly:font-load "ecrm10"))
		  ,(delay (ly:font-load "ecrm17")))))
     (#(roman upright bold) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "ecbx6"))
		  ,(delay (ly:font-load "ecbx8"))
		  ,(delay (ly:font-load "ecbx10"))
		  ,(delay (ly:font-load "ecbx12")))))
     (#(roman italic medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "ecti7"))
		  ,(delay (ly:font-load "ecti10"))
		  ,(delay (ly:font-load "ecti12")))))
     (#(roman italic bold) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "ecbi8"))
		  ,(delay (ly:font-load "ecbi10"))
		  ,(delay (ly:font-load "ecbi14")))))
     (#(roman caps medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "eccc10")))))
     (#(roman slanted-caps medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "ecsc10")))))
     (#(roman upright bold-narrow ) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "ecrb10")))))
     (#(sans upright medium) 
      . (,(ly:pt 10.0)  . #(,(delay (ly:font-load "ecss8"))
		   ,(delay (ly:font-load "ecss10"))
		   ,(delay (ly:font-load "ecss12"))
		   ,(delay (ly:font-load "ecss17")))))
     (#(typewriter upright medium) 
      . (,(ly:pt 10.0) . #(,(delay (ly:font-load "ectt8"))
		  ,(delay (ly:font-load "ectt10"))
		  ,(delay (ly:font-load "ectt12"))))))))

(define-public (add-pango-fonts node lily-family family factor)
  (define (add-node shape series)
    (add-font node
	      `((font-family . ,lily-family)
		(font-shape . ,shape)
		(font-series . ,series)
		(font-encoding . latin1) ;; ugh.
		)
	      
	      `(,(ly:pt (* factor 11.0))
		. #(,(cons
		     (ly:pt 12)
		     (ly:make-pango-description-string
		       `(((font-family . ,family)
			  (font-series . ,series)
			  (font-shape . ,shape)))
		       (ly:pt 12)))))))

  (add-node 'upright 'normal) 
  (add-node 'caps 'normal) 
  (add-node 'upright 'bold) 
  (add-node 'italic 'normal)
  (add-node 'italic 'bold))

(define-public (make-cmr-tree factor)
  (let*
      ((n (make-font-tree-node 'font-encoding 'fetaMusic))
       (module (resolve-module '(scm kpathsea)))
       (find (eval 'ly:kpathsea-find-file module))
       )
    (add-music-fonts n factor)
    (add-cmr-fonts n factor)
    
    (if (find "lmr10.pfb")
	(add-cork-lm-fonts n factor))
    (if (find "ecrm10.pfa")
	(add-ec-fonts n factor))
    n))




(define-public (make-pango-font-tree roman-str sans-str typewrite-str factor)
  (let ((n (make-font-tree-node 'font-encoding 'fetaMusic)))
    (add-music-fonts n factor)
    (add-pango-fonts n 'roman roman-str factor)
    (add-pango-fonts n 'sans sans-str factor)
    (add-pango-fonts n 'typewriter typewrite-str factor)
    n))


(define-public (make-century-schoolbook-tree factor)
  (make-pango-font-tree
    "Century Schoolbook L" 
    "Sans" "Mono" factor))

(define-public (magstep s)
  (exp (* (/ s 6) (log 2))))

(define-public (magnification->font-size m)
  (* 6 (/ (log m) (log 2))))
