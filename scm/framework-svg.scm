;;;; framework-svg.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-svg))

(use-modules (guile) (lily))
(use-modules (srfi srfi-2) (ice-9 regex))

;; FIXME: 0.62 to get paper size right
(define output-scale (* 0.62 scale-to-unit))

(define-public (output-framework outputter book scopes fields basename)
  (let* ((paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book))
	 (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t))
	 (page-number (1- (ly:output-def-lookup paper 'firstpagenumber)))
	 (page-count (length pages))
	 (hsize (ly:output-def-lookup paper 'hsize))
	 (vsize (ly:output-def-lookup paper 'vsize))
	 (page-width (inexact->exact (ceiling (* output-scale hsize))))
	 (page-height (inexact->exact (ceiling (* output-scale vsize)))))

    (ly:outputter-dump-string outputter xml-header)
    (ly:outputter-dump-string
     outputter
     (comment "Created with GNU LilyPond (http://lilypond.org)"))
    (ly:outputter-dump-string
     outputter (format #f "<svg id='svg1' width='~smm' height='~smm'>\n"
		       page-width page-height))
    (ly:outputter-dump-string
     outputter "<g transform='translate (10, 10) scale (1)'>\n")
      
;   (for-each
;    (lambda (x)
;      (ly:outputter-dump-string outputter x))
;    (cons
;     (page-header paper page-count)
;     (preamble paper)))
  
  (for-each
   (lambda (page)
     (set! page-number (1+ page-number))
     (dump-page outputter page page-number page-count landscape?))
   pages)
  (ly:outputter-dump-string outputter "\n</g>\n</svg>\n")))

(define (comment s)
  (string-append "<!-- " s " !-->\n"))

;; FIXME: gulp from file
(define xml-header
  "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 20010904//EN'
'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'>
")

(define (dump-page outputter page page-number page-count landscape?) 
  (ly:outputter-dump-string
   outputter
   (string-append
    (comment (format #f "Page: ~S/~S" page-number page-count))
    ;;(format #f "<g transform='translate (0, ~f)'>\n" (* output-scale y))))
    "<g>\n"))

  ;; FIXME:landscape
  (ly:outputter-dump-stencil outputter page)

  (ly:outputter-dump-string
   outputter
   (string-append
    (comment (format #f "End Page ~S/~S" page-number page-count))
    "</g>\n")))
