;;;; layout-page-dump.scm -- page breaking and page layout
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;	 2006 Nicolas Sceaux <nicolas.sceaux@free.fr>

(define-module (scm layout-page-dump)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (scm paper-system)
  #:use-module (scm page)
  #:use-module (scm layout-page-layout)
  #:use-module (lily)
  #:export (write-page-breaks
	    ;; utilities for writing other page dump functions
	    record-tweaks dump-all-tweaks))

(define (record-tweaks what property-pairs tweaks)
  (let ((key (ly:output-def-lookup (ly:grob-layout what)
				   'tweak-key
				   "tweaks"))
	(when (ly:grob-property what 'when)))
    (if (not (hash-ref tweaks key))
	(hash-set! tweaks key '()))
    (hash-set! tweaks key
	       (acons when property-pairs
		      (hash-ref tweaks key)))))

(define (graceless-moment mom)
  (ly:make-moment (ly:moment-main-numerator mom)
		  (ly:moment-main-denominator mom)
		  0 0))

(define (moment->skip mom)
  (let ((main (if (> (ly:moment-main-numerator mom) 0)
		  (format "\\skip 1*~a/~a"
			  (ly:moment-main-numerator mom)
			  (ly:moment-main-denominator mom))
		    ""))
	(grace (if (< (ly:moment-grace-numerator mom) 0)
		   (format "\\grace { \\skip 1*~a/~a }"
			   (- (ly:moment-grace-numerator mom))
			   (ly:moment-grace-denominator mom))
		   "")))
    (format "~a~a" main grace)))

(define (dump-tweaks out-port tweak-list last-moment)
  (if (not (null? tweak-list))
      (let* ((now (caar tweak-list))
	     (diff (ly:moment-sub now last-moment))
	     (these-tweaks (cdar tweak-list))
	     (skip (moment->skip diff))
	     (line-break-str (if (assoc-get 'line-break these-tweaks #f)
				 "\\break\n"
				 ""))
	     (page-break-str (if (assoc-get 'page-break these-tweaks #f)
				 "\\pageBreak\n"
				 ""))
	     (space-tweaks (format "\\spacingTweaks #'~a\n"
				   (with-output-to-string
				     (lambda ()
				       (pretty-print
					(assoc-get 'spacing-parameters
						   these-tweaks '()))))))
	     (base (format "~a~a~a"
			   line-break-str
			   page-break-str
			   space-tweaks)))
	(format out-port "~a\n~a\n" skip base)
	(dump-tweaks out-port (cdr tweak-list) (graceless-moment now)))))

(define (dump-all-tweaks pages tweaks output-name)
  (let* ((paper (ly:paper-book-paper (page-property (car pages) 'paper-book)))
	 (name (format "~a-page-layout.ly" output-name))
	 (out-port (open-output-file name)))
    
    (ly:message "Writing page layout to ~a" name)
    (hash-for-each
     (lambda (key val)
       (format out-port "~a = {" key)
       (dump-tweaks out-port (reverse val) (ly:make-moment 0 1))
       (display "}" out-port))
     tweaks)
    (close-port out-port)))

(define (write-page-breaks pages output-name)
  "Dump page breaks and tweaks"
  (let ((tweaks (make-hash-table 60)))
    (define (handle-page page)
      "Computes vertical stretch for each music line of `page' (starting by
      the smallest lines), then record the tweak parameters  of each line to
      the `tweaks' hash-table."
      (let* ((lines (page-property page 'lines))
	     (line-count (length lines))
	     (compute-max-stretch (ly:output-def-lookup
				   (ly:paper-book-paper (page-property page
								       'paper-book))
				    'system-maximum-stretch-procedure))
	     (page-number (page-property page 'page-number)))
	(let set-line-stretch! ((sorted-lines (sort lines
						    (lambda (l1 l2)
						      (< (line-height l1)
							 (line-height l2)))))
				(rest-height ;; sum of stretchable line heights
				 (reduce + 0.0
					 (map line-height
					      (filter stretchable-line? lines))))
				(space-left (page-maximum-space-left page)))
	  (if (not (null? sorted-lines))
	      (let* ((line (first sorted-lines))
		     (height (line-height line))
		     (stretch (min (compute-max-stretch line)
				   (if (and (stretchable-line? line)
					    (positive? rest-height))
				       (/ (* height space-left) rest-height)
				       0.0))))
		(set! (ly:prob-property line 'stretch) stretch)
		(set-line-stretch! (cdr sorted-lines)
				   (if (stretchable-line? line)
				       (- rest-height height)
				       rest-height)
				   (- space-left stretch)))))
	(let record-line-tweak ((lines lines)
				(is-first-line #t)
				(index 0))
	  (if (not (null? lines))
	      (let ((line (first lines)))
		(if (not (ly:prob-property? line 'is-title))
		    (record-tweaks
		     (ly:spanner-bound (ly:prob-property line 'system-grob) LEFT)
		     `((line-break . #t)
		       (page-break . ,is-first-line)
		       (spacing-parameters
			. ((page-number . ,page-number)
			   (system-index . ,index)
			   (system-stretch . ,(ly:prob-property line 'stretch))
			   (system-Y-extent . ,(paper-system-extent line Y))
			   (system-refpoint-Y-extent . ,(paper-system-staff-extents line))
			   (page-system-count . ,line-count)
			   (page-printable-height . ,(page-printable-height page))
			   (page-space-left . ,(page-property page 'space-left)))))
		     tweaks))
		(record-line-tweak (cdr lines) #f (1+ index)))))))
    ;; Compute tweaks for each page, then dump them to the page-layout file
    (for-each handle-page pages)
    (dump-all-tweaks pages tweaks output-name)))
