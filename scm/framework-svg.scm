;;;; framework-svg.scm -- structure for SVG output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                Patrick McCarty <pnorcks@gmail.com>

;;;; Recommendations:
;;;; http://www.w3.org/TR/SVG11/
;;;; http://www.w3.org/TR/SVGTiny12/
;;;;
;;;; Working draft:
;;;; http://www.w3.org/TR/SVGPrint/ -- for <pageSet> and <page>

;;;; TODO:
;;;; * Once <pageSet> and <page> are supported by Inkscape and
;;;;   other user agents, add a -d option (-dsvg-multiple-page)
;;;;   that will create a single SVG file containing all pages
;;;;   of output.  --pmccarty

(define-module (scm framework-svg))

(use-modules
  (guile)
  (lily)
  (scm page)
  (scm output-svg)
  (srfi srfi-1)
  (srfi srfi-2)
  (srfi srfi-13)
  (ice-9 regex))

(define format ergonomic-simple-format)

(define (svg-header paper)
  (let* ((lookup (lambda (x) (ly:output-def-lookup paper x)))
	 (unit-length (lookup 'output-scale))
	 (output-scale (* lily-unit->mm-factor unit-length))
	 (paper-width (lookup 'paper-width))
	 (paper-height (lookup 'paper-height))
	 (page-width (* output-scale paper-width))
	 (page-height (* output-scale paper-height)))

    `((xmlns . "http://www.w3.org/2000/svg")
      (xmlns:xlink . "http://www.w3.org/1999/xlink")
      (version . "1.2")
      (width . ,(ly:format "~2fmm" page-width))
      (height . ,(ly:format "~2fmm" page-height))
      (viewBox . ,(ly:format "0 0 ~4f ~4f"
			     paper-width paper-height)))))

(define (dump-page paper filename page page-number page-count)
  (let* ((outputter (ly:make-paper-outputter (open-file filename "wb") 'svg))
	 (dump (lambda (str) (display str (ly:outputter-port outputter)))))

    (dump (apply eo 'svg (svg-header paper)))
    (dump (comment (format "Page: ~S/~S" page-number page-count)))
    (ly:outputter-dump-stencil outputter page)
    (dump (ec 'svg))
    (ly:outputter-close outputter)))

(define (output-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
	 (page-stencils (map page-stencil (ly:paper-book-pages book)))
	 (page-number (1- (ly:output-def-lookup paper 'first-page-number)))
	 (page-count (length page-stencils))
	 (filename "")
	 (file-suffix (lambda (num)
			(if (= page-count 1) "" (format "-page-~a" num)))))

    (for-each
      (lambda (page)
	(set! page-number (1+ page-number))
	(set! filename (format "~a~a.svg"
			       basename
			       (file-suffix page-number)))
	(dump-page paper filename page page-number page-count))
      page-stencils)))
