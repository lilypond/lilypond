;;;; framework-svg.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-svg))

(use-modules (guile) (lily) (scm output-svg))
(use-modules (srfi srfi-1) (srfi srfi-2) (srfi srfi-13) (ice-9 regex))

;; FIXME: 0.62 to get paper size right
(define output-scale (* 0.62 scale-to-unit))

(define-public (output-framework basename book scopes fields)
  (let* ((filename (format "~a.svg" basename))
	 (outputter  (ly:make-paper-outputter filename
					      (ly:output-backend)))
	 (paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book))
	 (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t))
	 (page-number (1- (ly:output-def-lookup paper 'firstpagenumber)))
	 (page-count (length pages))
	 (hsize (ly:output-def-lookup paper 'hsize))
	 (vsize (ly:output-def-lookup paper 'vsize))
	 (page-width (inexact->exact (ceiling (* output-scale hsize))))
	 (page-height (inexact->exact (ceiling (* output-scale vsize))))
	 (page-set? (or (> page-count 1) landscape?)))
    
   (ly:outputter-dump-string
    outputter
     (eo 'svg
	 '(xmlns . "http://www.w3.org/2000/svg")
	 '(version . "1.2")
	 `(width . ,(format #f "~smm" page-width))
	 `(height . ,(format #f "~smm" page-height))))
   
   (ly:outputter-dump-string outputter (dump-fonts outputter paper))
   (ly:outputter-dump-string
    outputter
    (string-append
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
  (ly:outputter-dump-string
   outputter (comment (format #f "Page: ~S/~S" page-number page-count)))
  (if (or landscape? page-set?)
      (ly:outputter-dump-string
       outputter
       (if landscape? (eo 'page '(page-orientation . "270")) (eo 'page))))
  (ly:outputter-dump-string outputter (string-append (eo 'g)))
  (ly:outputter-dump-stencil outputter page)
  (ly:outputter-dump-string outputter (string-append (ec 'g)))
  (if (or landscape? page-set?)
      (ly:outputter-dump-string outputter (ec 'page))))

(define (embed-font string)
  (let ((start (string-contains string "<defs>"))
	(end (string-contains string "</defs>")))
    (substring string (+ start 7) (- end 1))))

(define (dump-fonts outputter paper)
  (let* ((fonts (ly:paper-fonts paper))
	 (font-names (uniq-list (sort
				 (filter string?
					 (map ly:font-file-name fonts)) string<?)))
	 (svgs (map
		(lambda (x)
		  (let ((file-name (ly:find-file (string-append x ".svg"))))
		    (if file-name (embed-font (ly:gulp-file file-name))
			(begin (ly:warn "cannot find SVG font ~S" x) ""))))
		(filter string? font-names))))
    (entity 'defs (string-join svgs "\n"))))

