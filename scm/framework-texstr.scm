;;;; framework-tex.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c)  2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-module (scm framework-texstr)
  #:export (output-framework-tex	
	    output-classic-framework-tex))

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define (dump-page putter page last? with-extents?)
  (ly:outputter-dump-string
   putter
   (format "\\lybox{~a}{~a}{%\n"
	   (if with-extents?
	       (interval-start (ly:stencil-extent page X))
	       0.0)
	   (if with-extents?
	       (- (interval-start (ly:stencil-extent page Y)))
	       0.0)))
  (ly:outputter-dump-string
   putter
   (if last?
       "}%\n\\vfill\n"
       "}%\n\\vfill\n\\lilypondpagebreak\n")))

(define-public (output-framework outputter book scopes fields basename )
  (let* ((paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book))
	 )
    (for-each
     (lambda (x)
       (ly:outputter-dump-string outputter x))
     (list))
    (for-each
     (lambda (page)
       (ly:outputter-dump-stencil outputter page))
     pages)))

(define (dump-line putter line last?)
  (ly:outputter-dump-string
   putter
   (format "\\lybox{~a}{~a}{%\n"
	   (ly:number->string
	    (max 0 (interval-end (ly:paper-system-extent line X))))
	   (ly:number->string
	    (interval-length (ly:paper-system-extent line Y)))))

  (ly:outputter-dump-stencil putter (ly:paper-system-stencil line))
  (ly:outputter-dump-string
   putter
   (if last?
       "}%\n"
       "}\\interscoreline\n")))


(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
(define-public (convert-to-dvi . args) #t)
(define-public (convert-to-tex . args) #t)
