;;
;; framework-svg.scm -- structure for SVG output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2005 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-svg))

(use-modules (guile)
	     (lily)
	     (scm output-svg))

(use-modules (srfi srfi-1)
	     (srfi srfi-2)
	     (srfi srfi-13)
	     (ice-9 regex))

(define-public (output-framework basename book scopes fields)
  (let* ((filename (format "~a.svg" basename))
	 (outputter  (ly:make-paper-outputter (open-file filename "wb")
					      (ly:output-backend)))
	 (dump (lambda (str) (display str (ly:outputter-port outputter))))
   	 (paper (ly:paper-book-paper book))
	 (unit-length (ly:output-def-lookup paper 'outputscale))
	 (output-scale (* lily-unit->mm-factor
			  unit-length))
	 (pages (ly:paper-book-pages book))
	 (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t))
	 (page-number (1- (ly:output-def-lookup paper 'firstpagenumber)))
	 (page-count (length pages))
	 (hsize (ly:output-def-lookup paper 'hsize))
	 (vsize (ly:output-def-lookup paper 'vsize))
	 (page-width (inexact->exact (ceiling (* output-scale hsize))))
	 (page-height (inexact->exact (ceiling (* output-scale vsize))))
	 (page-set? (or (> page-count 1) landscape?)))

    (ly:outputter-output-scheme outputter
				`(begin (set! lily-unit-length ,unit-length) ""))
    (dump (eo 'svg
	      '(xmlns . "http://www.w3.org/2000/svg")
	      '(xmlns:xlink . "http://www.w3.org/1999/xlink")
	      '(version . "1.2")

	      ;; Argggghhhh: SVG takes the px <-> mm mapping from the windowing system
	      `(width . ,(format #f "~s" page-width))
	      `(height . ,(format #f "~s" page-height))))
    
    (dump (dump-fonts outputter paper))
    (dump
     (string-append
      ;; FIXME: only use pages if there are more than one, pageSet is
      ;; not supported by all SVG applications yet.
      (if page-set? (eo 'pageSet) "")
      (eo 'g `(transform . ,(format "scale(~a, ~a) "
				    output-scale output-scale)))))
    
    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (dump-page outputter page page-number page-count landscape? page-set?))
     pages)
    
    (if page-set? (eo 'pageSet) "")
    (dump
     (string-append
      (ec 'g)
      (if page-set? (ec 'pageSet) "")
      (ec 'svg)))))

(define (dump-page outputter page page-number page-count landscape? page-set?)
  (define (dump str) (display str (ly:outputter-port outputter)))
  
  (dump (comment (format #f "Page: ~S/~S" page-number page-count)))
  (if (or landscape? page-set?)
      (dump
       (if landscape?
	   (eo 'page '(page-orientation . "270"))
	   (eo 'page))))
  
  (dump (string-append (eo 'g )))
  (ly:outputter-dump-stencil outputter page)
  (dump (string-append (ec 'g)))
  (if (or landscape? page-set?)
      (dump (ec 'page))))

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
		    (if file-name (embed-font (cached-file-contents file-name))
			(begin (ly:warning "cannot find SVG font ~S" x) ""))))
		(filter string? font-names))))
    (entity 'defs (string-join svgs "\n"))))

