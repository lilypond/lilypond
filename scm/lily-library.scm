

(define-public X 0)
(define-public Y 1)
(define-public START -1)
(define-public STOP 1)
(define-public LEFT -1)
(define-public RIGHT 1)
(define-public UP 1)
(define-public DOWN -1)
(define-public CENTER 0)

(define-public DOUBLE-FLAT -4)
(define-public THREE-Q-FLAT -3)
(define-public FLAT -2)
(define-public SEMI-FLAT -1)
(define-public NATURAL 0)
(define-public SEMI-SHARP 1)
(define-public SHARP 2)
(define-public THREE-Q-SHARP 3)
(define-public DOUBLE-SHARP 4)
(define-public SEMI-TONE 2)

(define-public ZERO-MOMENT (ly:make-moment 0 1)) 

(define-public (moment-min a b)
  (if (ly:moment<? a b) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lily specific variables.

(define-public default-script-alist '())


;; parser stuff.
(define-public (print-music-as-book parser music)
  (let* ((head  (ly:parser-lookup parser '$globalheader))
	 (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			     head score)))
    (ly:parser-print-book parser book)))

(define-public (print-score-as-book parser score)
  (let*
      ((head  (ly:parser-lookup parser '$globalheader))
       (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			   head score)))
    (ly:parser-print-book parser book)))

(define-public (print-score parser score)
  (let* ((head  (ly:parser-lookup parser '$globalheader))
	 (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			     head score)))
    (ly:parser-print-score parser book)))
		
(define-public (collect-scores-for-book  parser score)
  (let*
      ((oldval (ly:parser-lookup parser 'toplevel-scores)))
    (ly:parser-define parser 'toplevel-scores (cons score oldval))
    ))

(define-public (collect-music-for-book parser music)
  (collect-scores-for-book parser (ly:music-scorify music parser)))


  
;;;;;;;;;;;;;;;;
; alist
(define-public assoc-get ly:assoc-get)

(define-public (uniqued-alist alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))

(define-public (alist<? x y)
  (string<? (symbol->string (car x))
	    (symbol->string (car y))))

(define-public (chain-assoc x alist-list)
  (if (null? alist-list)
      #f
      (let* ((handle (assoc x (car alist-list))))
	(if (pair? handle)
	    handle
	    (chain-assoc x (cdr alist-list))))))

(define-public (chain-assoc-get x alist-list . default)
  "Return ALIST entry for X. Return DEFAULT (optional, else #f) if not
found."

  (define (helper x alist-list default)
    (if (null? alist-list)
	default
	(let* ((handle (assoc x (car alist-list))))
	  (if (pair? handle)
	      (cdr handle)
	      (helper x (cdr alist-list) default)))))

  (helper x alist-list
	  (if (pair? default) (car default) #f)))

(define (map-alist-vals func list)
  "map FUNC over the vals of  LIST, leaving the keys."
  (if (null?  list)
      '()
      (cons (cons  (caar list) (func (cdar list)))
	    (map-alist-vals func (cdr list)))
      ))

(define (map-alist-keys func list)
  "map FUNC over the keys of an alist LIST, leaving the vals. "
  (if (null?  list)
      '()
      (cons (cons (func (caar list)) (cdar list))
	    (map-alist-keys func (cdr list)))
      ))
 
;;;;;;;;;;;;;;;;
;; hash



(if (not (defined? 'hash-table?))	; guile 1.6 compat
    (begin
      (define hash-table? vector?)

      (define-public (hash-table->alist t)
	"Convert table t to list"
	(apply append
	       (vector->list t)
	       )))

    ;; native hashtabs.
    (begin
      (define-public (hash-table->alist t)

	(hash-fold (lambda (k v acc) (acons  k v  acc))
		   '() t)
	)
      ))

;; todo: code dup with C++. 
(define-public (alist->hash-table l)
  "Convert alist to table"
  (let
      ((m (make-hash-table (length l))))

    (map (lambda (k-v)
	   (hashq-set! m (car k-v) (cdr k-v)))
	 l)

    m))
       



;;;;;;;;;;;;;;;;
; list

(define (flatten-list lst)
  "Unnest LST" 
  (if (null? lst)
      '()
      (if (pair? (car lst))
	  (append (flatten-list (car lst)) (flatten-list  (cdr lst)))
	  (cons (car lst) (flatten-list (cdr lst))))
  ))

(define (list-minus a b)
  "Return list of elements in A that are not in B."
  (lset-difference eq? a b))


;; TODO: use the srfi-1 partition function.
(define-public (uniq-list l)
  
  "Uniq LIST, assuming that it is sorted"
  (define (helper acc l) 
    (if (null? l)
	acc
	(if (null? (cdr l))
	    (cons (car l) acc)
	    (if (equal? (car l) (cadr l))
		(helper acc (cdr l))
		(helper (cons (car l) acc)  (cdr l)))
	    )))
  (reverse! (helper '() l) '()))


(define (split-at-predicate predicate l)
 "Split L = (a_1 a_2 ... a_k b_1 ... b_k)
into L1 = (a_1 ... a_k ) and L2 =(b_1 .. b_k) 
Such that (PREDICATE a_i a_{i+1}) and not (PREDICATE a_k b_1).
L1 is copied, L2 not.

(split-at-predicate (lambda (x y) (= (- y x) 2))  '(1 3 5 9 11) (cons '() '()))"
;; "

;; KUT EMACS MODE.

  (define (inner-split predicate l acc)
  (cond
   ((null? l) acc)
   ((null? (cdr l))
    (set-car! acc (cons (car l) (car acc)))
    acc)
   ((predicate (car l) (cadr l))
    (set-car! acc (cons (car l) (car acc)))
    (inner-split predicate (cdr l) acc))
   (else
    (set-car! acc (cons (car l) (car acc)))
    (set-cdr! acc (cdr l))
    acc)

  ))
 (let*
    ((c (cons '() '()))
     )
  (inner-split predicate l  c)
  (set-car! c (reverse! (car c))) 
  c)
)


(define-public (split-list l sep?)
"
(display (split-list '(a b c / d e f / g) (lambda (x) (equal? x '/))) )
=>
((a b c) (d e f) (g))

"
;; " KUT EMACS.

(define (split-one sep?  l acc)
  "Split off the first parts before separator and return both parts."
  (if (null? l)
      (cons acc '())
      (if (sep? (car l))
	  (cons acc (cdr l))
	  (split-one sep? (cdr l) (cons (car l) acc))
	  )
      ))

(if (null? l)
    '()
    (let* ((c (split-one sep? l '())))
      (cons (reverse! (car c) '()) (split-list (cdr c) sep?))
      )))


(define-public (interval-length x)
  "Length of the number-pair X, when an interval"
  (max 0 (- (cdr x) (car x)))
  )
(define-public interval-start car)
(define-public interval-end cdr)

(define (other-axis a)
  (remainder (+ a 1) 2))
  

(define-public (interval-widen iv amount)
   (cons (- (car iv) amount)
         (+ (cdr iv) amount)))

(define-public (interval-union i1 i2)
   (cons (min (car i1) (car i2))
	 (max (cdr i1) (cdr i2))))


(define-public (write-me message x)
  "Return X.  Display MESSAGE and write X.  Handy for debugging,
possibly turned off."
  (display message) (write x) (newline) x)
;;  x)

(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define (cons-map f x)
  "map F to contents of X"
  (cons (f (car x)) (f (cdr x))))


(define-public (list-insert-separator lst between)
  "Create new list, inserting BETWEEN between elements of LIST"
  (define (conc x y )
    (if (eq? y #f)
	(list x)
	(cons x  (cons between y))
	))
  (fold-right conc #f lst))

;;;;;;;;;;;;;;;;
; other
(define (sign x)
  (if (= x 0)
      0
      (if (< x 0) -1 1)))

(define-public (symbol<? l r)
  (string<? (symbol->string l) (symbol->string r)))

(define-public (!= l r)
  (not (= l r)))


