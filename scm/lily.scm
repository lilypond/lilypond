;;;; lily.scm -- implement Scheme output routines for TeX and PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;; Library functions

(use-modules (ice-9 regex))

;;(write standalone (current-error-port))

;;; General settings

(debug-enable 'backtrace)


(define point-and-click #f)
(define security-paranoia #f)
(define midi-debug #f)

(define (line-column-location line col file)
  "Print an input location, including column number ."
  (string-append (number->string line) ":"
		 (number->string col) " " file)
  )

(define (line-location line col file)
  "Print an input location, without column number ."
  (string-append (number->string line) " " file)
  )

;; cpp hack to get useful error message
(define ifdef "First run this through cpp.")
(define ifndef "First run this through cpp.")
  
(define default-script-alist '())
(define font-name-alist  '())

(if (not (defined? 'standalone))
    (define standalone (not (defined? 'ly-gulp-file))))

;; The regex module may not be available, or may be broken.
(define use-regex
  (let ((os (string-downcase (vector-ref (uname) 0))))
    (not (equal? "cygwin" (substring os 0 (min 6 (string-length os)))))))

;; If you have trouble with regex, define #f
(define use-regex #t)
;;(define use-regex #f)


;;; Un-assorted stuff

;; URG guile-1.4/1.4.x compatibility
(define (ly-eval x) (eval2 x #f))

(define (sign x)
  (if (= x 0)
      1
      (if (< x 0) -1 1)))

(define (write-me n x)
  (display n)
  (write x)
  (newline)
  x)

(define (empty? x)
  (equal? x '()))

(define (!= l r)
  (not (= l r)))

(define (filter-list pred? list)
  "return that part of LIST for which PRED is true."
  (if (null? list) '()
      (let* ((rest  (filter-list pred? (cdr list))))
	(if (pred?  (car list))
	    (cons (car list)  rest)
	    rest))))

(define (filter-out-list pred? list)
  "return that part of LIST for which PRED is true."
  (if (null? list) '()
      (let* ((rest  (filter-list pred? (cdr list))))
	(if (not (pred?  (car list)))
	    (cons (car list)  rest)
	    rest))))

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))

(define (uniq-list list)
  (if (null? list) '()
      (if (null? (cdr list))
	  list
	  (if (equal? (car list) (cadr list))
	      (uniq-list (cdr list))
	      (cons (car list) (uniq-list (cdr list)))))))

(define (alist<? x y)
  (string<? (symbol->string (car x))
	    (symbol->string (car y))))


(map (lambda (x) (eval-string (ly-gulp-file x)))
     '("output-lib.scm"
       "tex.scm"
       "ps.scm"
       "pdf.scm"
       "pdftex.scm"
       "ascii-script.scm"
       ))

(if (not standalone)
    (map (lambda (x) (eval-string (ly-gulp-file x)))
	 '("c++.scm"
	   "grob-property-description.scm"
	   "translator-property-description.scm"
	   "interface-description.scm"
	   "beam.scm"
	   "clef.scm"
	   "slur.scm"
	   "font.scm"
	   "music-functions.scm"
	   "music-property-description.scm"
	   "auto-beam.scm"
	   "generic-property.scm"
	   "basic-properties.scm"
	   "chord-name.scm"
	   "grob-description.scm"
	   "script.scm"
	   "drums.scm"
	   "midi.scm"
	   )))


