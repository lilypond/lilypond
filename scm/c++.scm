;;;; c++.scm -- implement Scheme frontends to C++ functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;; Note: this file can't be used without LilyPond executable

(define (number-pair?  x)
  (and (pair? x)
       (number? (car x)) (number? (cdr x))))

(define (moment-pair?  x)
  (and (pair? x)
       (moment? (car x)) (moment? (cdr x))))

(define (boolean-or-symbol? x)
  (or (boolean? x) (symbol? x)))

(define (number-or-boolean? x)
  (or (number? x) (boolean? x)))

(define (number-or-string? x)
  (or (number? x) (string? x)))

(define (markup? x)
  (or (string? x) (list? x)))

;; ugh: code dup ; merge.
(define (object-type obj)
  (cond
   ((dir? obj) "direction")
   ((number-pair? obj) "pair of numbers")
   ((ly-input-location? obj) "input location")   
   ((ly-grob? obj) "grob (GRaphical OBject)")
   ((pair? obj) "pair")
   ((integer? obj) "integer")
   ((list? obj) "list")
   ((symbol? obj) "symbol")
   ((string? obj) "string")
   ((boolean? obj) "boolean")
   ((moment? obj) "moment")
   ((number? obj) "number")
   ((char? obj) "char")
   ((input-port? obj) "input port")
   ((output-port? obj) "output port")   
   ((vector? obj) "vector")
   ((procedure? obj) "procedure") 
   ((boolean-or-symbol? obj) "boolean or symbol")
   ((number-or-string? obj) "number or string")
   ((number-or-boolean? obj) "number or boolean")
   ((markup? obj) "markup (list or string)")
   (else "unknown type")))


(define (type-name  predicate)
  (cond
   ((eq? predicate dir?) "direction")
   ((eq? predicate number-pair?) "pair of numbers")
   ((eq? predicate ly-input-location?) "input location")   
   ((eq? predicate ly-grob?) "Grob")
   ((eq? predicate pair?) "pair")
   ((eq? predicate integer?) "integer")
   ((eq? predicate list?) "list")
   ((eq? predicate symbol?) "symbol")
   ((eq? predicate string?) "string")
   ((eq? predicate boolean?) "boolean")
   ((eq? predicate moment?) "moment")
   ((eq? predicate number?) "number")
   ((eq? predicate char?) "char")
   ((eq? predicate input-port?) "input port")
   ((eq? predicate output-port?) "output port")   
   ((eq? predicate vector?) "vector")
   ((eq? predicate procedure?) "procedure") 
   ((eq? predicate boolean-or-symbol?) "boolean or symbol")
   ((eq? predicate number-or-string?) "number or string")
   ((eq? predicate markup?) "markup (list or string)")
   (else "unknown type")))


(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))



;;(define (cons-map f x)
;;  (cons (f (car x)) (f (cdr x))))


;;(define (reduce operator list)
;;      (if (null? (cdr list)) (car list)
;;	  (operator (car list) (reduce operator (cdr list)))))



; Make a function that checks score element for being of a specific type. 
(define (make-type-checker symbol)
  (lambda (elt)
    ;;(display  symbol)
    ;;(eq? #t (ly-get-elt-property elt symbol))
    (not (eq? #f (memq symbol (ly-get-elt-property elt 'interfaces))))))


(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define (repeat-name-to-ctor name)
  (let*
      ((supported-reps
	`(("volta" . ((iterator-ctor . ,Volta_repeat_iterator::constructor)
		      (length . ,Repeated_music::volta_music_length)))
	    ("unfold" . ((iterator-ctor . ,Unfolded_repeat_iterator::constructor)
		       (length . ,Repeated_music::unfolded_music_length)))
	    ("fold" . ((iterator-ctor  . ,Folded_repeat_iterator::constructor)
		       (length . ,Repeated_music::folded_music_length)))
	    ("tremolo" . ((iterator-ctor . ,Chord_tremolo_iterator::constructor)
			  (length . ,Repeated_music::unfolded_music_length)))))
	  
       (handle (assoc name supported-reps)))

    (if (pair? handle)
	(cdr handle)
	(begin
	  (ly-warn
	   (string-append "Unknown repeat type `" name "'\nSee scm/lily.scm for supported repeats"))
	  '(type . 'repeated-music)))))
