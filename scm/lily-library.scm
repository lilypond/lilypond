;;;; lily-library.scm -- utilities
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


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

(define-public (average x . lst)
  (/ (+ x (apply + lst)) (1+ (length lst))))

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
  (let* ((head (ly:parser-lookup parser '$globalheader))
	 (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			     head score)))
    (ly:parser-print-book parser book)))

(define-public (print-score parser score)
  (let* ((head  (ly:parser-lookup parser '$globalheader))
	 (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			     head score)))
    (ly:parser-print-score parser book)))
		
(define-public (collect-scores-for-book  parser score)
  (let* ((oldval (ly:parser-lookup parser 'toplevel-scores)))
    (ly:parser-define parser 'toplevel-scores (cons score oldval))))

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
	    (map-alist-vals func (cdr list)))))

(define (map-alist-keys func list)
  "map FUNC over the keys of an alist LIST, leaving the vals. "
  (if (null?  list)
      '()
      (cons (cons (func (caar list)) (cdar list))
	    (map-alist-keys func (cdr list)))))
 
;;;;;;;;;;;;;;;;
;; vector
(define-public (vector-for-each proc vec)
  (do
      ((i 0 (1+ i)))
      ((>= i (vector-length vec)) vec)
    (vector-set! vec i (proc (vector-ref vec i)))))

;;;;;;;;;;;;;;;;
;; hash

(if (not (defined? 'hash-table?)) ;; guile 1.6 compat
    (begin
      (define hash-table? vector?)

      (define-public (hash-table->alist t)
	"Convert table t to list"
	(apply append (vector->list t))))

    ;; native hashtabs.
    (begin
      (define-public (hash-table->alist t)
	(hash-fold (lambda (k v acc) (acons  k v  acc))
		   '() t))))

;; todo: code dup with C++. 
(define-public (alist->hash-table lst)
  "Convert alist to table"
  (let ((m (make-hash-table (length lst))))
    (map (lambda (k-v) (hashq-set! m (car k-v) (cdr k-v))) lst)
    m))

;;;;;;;;;;;;;;;;
; list

(define (flatten-list lst)
  "Unnest LST" 
  (if (null? lst)
      '()
      (if (pair? (car lst))
	  (append (flatten-list (car lst)) (flatten-list  (cdr lst)))
	  (cons (car lst) (flatten-list (cdr lst))))))

(define (list-minus a b)
  "Return list of elements in A that are not in B."
  (lset-difference eq? a b))

;; TODO: use the srfi-1 partition function.
(define-public (uniq-list lst)
  
  "Uniq LST, assuming that it is sorted"
  (define (helper acc lst) 
    (if (null? lst)
	acc
	(if (null? (cdr lst))
	    (cons (car lst) acc)
	    (if (equal? (car lst) (cadr lst))
		(helper acc (cdr lst))
		(helper (cons (car lst) acc)  (cdr lst))))))
  (reverse! (helper '() lst) '()))

(define (split-at-predicate predicate lst)
 "Split LST = (a_1 a_2 ... a_k b_1 ... b_k)
  into L1 = (a_1 ... a_k ) and L2 =(b_1 .. b_k) 
  Such that (PREDICATE a_i a_{i+1}) and not (PREDICATE a_k b_1).
  L1 is copied, L2 not.

  (split-at-predicate (lambda (x y) (= (- y x) 2)) '(1 3 5 9 11) (cons '() '()))"
 ;; " Emacs is broken

 (define (inner-split predicate lst acc)
   (cond
    ((null? lst) acc)
    ((null? (cdr lst))
     (set-car! acc (cons (car lst) (car acc)))
     acc)
    ((predicate (car lst) (cadr lst))
     (set-car! acc (cons (car lst) (car acc)))
     (inner-split predicate (cdr lst) acc))
    (else
     (set-car! acc (cons (car lst) (car acc)))
     (set-cdr! acc (cdr lst))
     acc)))
 
 (let* ((c (cons '() '())))
   (inner-split predicate lst  c)
   (set-car! c (reverse! (car c)))
   c))

(define-public (split-list lst sep?)
   "(display (split-list '(a b c / d e f / g) (lambda (x) (equal? x '/))))
   =>
   ((a b c) (d e f) (g))
  "
   ;; " Emacs is broken
   (define (split-one sep?  lst acc)
     "Split off the first parts before separator and return both parts."
     (if (null? lst)
	 (cons acc '())
	 (if (sep? (car lst))
	     (cons acc (cdr lst))
	     (split-one sep? (cdr lst) (cons (car lst) acc)))))
   
   (if (null? lst)
       '()
       (let* ((c (split-one sep? lst '())))
	 (cons (reverse! (car c) '()) (split-list (cdr c) sep?)))))

(define-public (offset-add a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b)))) 

(define-public (ly:list->offsets accum coords)
  (if (null? coords)
      accum
      (cons (cons (car coords) (cadr coords))
	    (ly:list->offsets accum (cddr coords)))))

(define-public (interval-length x)
  "Length of the number-pair X, when an interval"
  (max 0 (- (cdr x) (car x))))

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

(define-public (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

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
	(cons x  (cons between y))))
  (fold-right conc #f lst))

;;;;;;;;;;;;;;;;
; other
(define (sign x)
  (if (= x 0)
      0
      (if (< x 0) -1 1)))

(define-public (symbol<? lst r)
  (string<? (symbol->string lst) (symbol->string r)))

(define-public (!= lst r)
  (not (= lst r)))

(define-public scale-to-unit
  (cond
   ((equal? (ly:unit) "mm") (/ 72.0 25.4))
   ((equal? (ly:unit) "pt") (/ 72.0 72.27))
   (else (error "unknown unit" (ly:unit)))))

;;; font
(define-public (font-name-style font)
  ;; FIXME: ughr, (ly:font-name) sometimes also has Style appended.
  (let* ((font-name (ly:font-name font))
	 (full-name (if font-name font-name (ly:font-file-name font)))
	 (name-style (string-split full-name #\-)))
    ;; FIXME: ughr, barf: feta-alphabet is actually emmentaler
    (if (string-prefix? "feta-alphabet" full-name)
	(list "emmentaler"
	      (substring  full-name (string-length "feta-alphabet")))
	(if (not (null? (cdr name-style)))
	    name-style
	    (append name-style '("Regular"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ps-embed-cff body font-set-name version)
  (let* ((binary-data
	  (string-append
	   (format "/~a ~s StartData " font-set-name (string-length body))
	   body))

	 (header
	  (format
	   "%%BeginResource: font ~a
%!PS-Adobe-3.0 Resource-FontSet
%%DocumentNeededResources: ProcSet (FontSetInit)
%%Title: (FontSet/~a)
%%Version: ~s
%%EndComments
%%IncludeResource: ProcSet (FontSetInit)
%%BeginResource: FontSet (~a)
/FontSetInit /ProcSet findresource begin
%%BeginData: ~s Binary Bytes
"
	   font-set-name font-set-name version font-set-name
	   (string-length binary-data)))
	 (footer "\n%%EndData
%%EndResource
%%EOF
%%EndResource"))

    (string-append
     header
     binary-data
     footer)))
