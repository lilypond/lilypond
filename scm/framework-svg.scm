;;;; framework-svg.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-svg))

(use-modules (guile) (lily) (scm output-svg))
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
	 (page-height (inexact->exact (ceiling (* output-scale vsize))))
	 (page-set? (> page-count 1)))
    
   (ly:outputter-dump-string
    outputter
    (string-append
     (eo 'svg
	 '(xmlns . "http://www.w3.org/2000/svg")
	 '(version . "1.2")
	 `(width . ,(format #f "~smm" page-width))
	 `(height . ,(format #f "~smm" page-height)))
     ;; FIXME: only use pages if there are more than one, pageSet is
     ;; not supported by all SVG applications yet.
     (if page-set? (eo 'pageSet) "")
     (eo 'g)))
       
  (for-each
   (lambda (page)
     (set! page-number (1+ page-number))
     (dump-page outputter page page-number page-count landscape? page-set?))
   pages)
  
  (if page-set? (eo 'pageSet) "")
  (ly:outputter-dump-string
   outputter
   (string-append
    (ec 'g)
    (if page-set? (ec 'pageSet) "")
    (ec 'svg)))))

(define (dump-page outputter page page-number page-count landscape? page-set?)
  ;; FIXME:landscape
  (ly:outputter-dump-string
   outputter (comment (format #f "Page: ~S/~S" page-number page-count)))
  (if page-set? (ly:outputter-dump-string outputter (eo 'page)))
  (ly:outputter-dump-string outputter (string-append (eo 'g)))
  (ly:outputter-dump-stencil outputter page)
  (ly:outputter-dump-string outputter (string-append (ec 'g)))
  (if page-set? (ly:outputter-dump-string outputter (ec 'page))))
