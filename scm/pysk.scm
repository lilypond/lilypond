;;; pysk.scm -- implement Python  output routines (for Sketch)
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>



(define-module (scm pysk)
  :export (pysk-output-expression)
  :no-backtrace
  )

(use-modules (scm ps)
	     (ice-9 regex)
	     (ice-9 string-fun)
	     (guile-user)
	     (guile)
	     )

(define this-module (current-module))
(define (pysk-output-expression expr port)
  (display (pythonify expr) port )
  )

(define (ly-warn s) (display s))

(define (pythonify q)
  (cond
   ((string? q) (py-str q))
   ((symbol? q) (py-str (symbol->string q)))
   ((and (pair?  q)
	 (not (pair? (cdr q)))
	 (not (eq? '() (cdr q)))
	 ) (py-tuple q))
   ((pair? q) (py-listify q))
   ((number? q) (number->string q))
   ((eq? q '()) '())
   (else (begin
	   (ly-warn "Unknown object to pythonify:")
	   (write q)
	   (newline)
	   )
  )))

(define (py-str s)
  (string-append "'" s "'")
  )

(define (py-tuple q)
  (string-append "(" (pythonify (car  q)) "," (pythonify (cdr q)) ")")
  )

(define (reduce-list list between)
  "Create new list, inserting BETWEEN between elements of LIST"
  (if (null? list)
      '()
      (if (null? (cdr list))
	  list
	  (cons (car list)
		(cons between (reduce-list (cdr list) between)))
  
  )))

(define (string-join str-list sep)
  (apply string-append (reduce-list str-list sep))
  )

(define (my-map f l)
  (if (null? l)
      '()
      (if (pair? (cdr l))
	  (cons (f (car l)) (my-map f (cdr l)))
	  (cons (f (car l)) (f (cdr l)))
	  )
  ))

(define (tuplify-list lst)
  (if (null? lst)
      '()
      (if (pair? (cdr lst))
	  (cons (car lst) (tuplify-list (cdr lst)))
	  (if (eq? '() (cdr lst))
	      lst
	      (list (string-append "(" (car lst) ", " (cdr lst) ")" ))
	      ))
	  ))

(define (py-listify q)
  (string-append
   "["
   (string-join
    (tuplify-list (my-map pythonify q))   ",")
   "]\n"
   ))


