;;;; c++.scm -- implement Scheme frontends to C++ functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;; Note: this file can't be used without LilyPond executable


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type predicates.
(define-public (number-pair?  x)
  (and (pair? x)
       (number? (car x)) (number? (cdr x))))
(define-public (number-or-grob? x)
  (or (ly-grob? x) (number? x))
  )

(define-public (grob-list? x)
  (list? x))

(define-public (moment-pair?  x)
  (and (pair? x)
       (moment? (car x)) (moment? (cdr x))))

(define-public (boolean-or-symbol? x)
  (or (boolean? x) (symbol? x)))

(define-public (number-or-string? x)
  (or (number? x) (string? x)))

(define-public (markup? x)
  (or (string? x) (list? x)))

(define-public (scheme? x) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define type-p-name-alist
  `(
   (,dir? . "direction")
   (,scheme? . "any type")
   (,number-pair? . "pair of numbers")
   (,ly-input-location? . "input location")   
   (,ly-grob? . "grob (GRaphical OBject)")
   (,grob-list? . "list of grobs")
   (,duration? . "duration")
   (,pair? . "pair")
   (,integer? . "integer")
   (,list? . "list")
   (,symbol? . "symbol")
   (,string? . "string")
   (,boolean? . "boolean")
   (,moment? . "moment")
   (,ly-input-location? . "input location")
   (,music-list? . "list of music")
   (,music? . "music")
   (,number? . "number")
   (,char? . "char")
   (,input-port? . "input port")
   (,output-port? . "output port")   
   (,vector? . "vector")
   (,procedure? . "procedure") 
   (,boolean-or-symbol? . "boolean or symbol")
   (,number-or-string? . "number or string")
   (,markup? . "markup (list or string)")
   (,number-or-grob? . "number or grob")
   ))



(define (match-predicate obj alist)
  (if (null? alist)
      "Unknown type"
      (if (apply (caar alist) obj)
	  (cdar alist)
	  (match-predicate obj (cdr alist))
	  )
      ))

(define-public (object-type obj)
  (match-predicate obj type-p-name-alist))

(define-public (type-name  predicate)
  (let ((entry (assoc predicate type-p-name-alist)))
    (if (pair? entry) (cdr entry)
	"unknown"
	)))
