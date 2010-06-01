;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2010 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                Patrick McCarty <pnorcks@gmail.com>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

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
  (scm paper-system)
  (scm output-svg)
  (srfi srfi-1)
  (srfi srfi-2)
  (srfi srfi-13)
  (ice-9 regex))

(define format ergonomic-simple-format)

(define (svg-begin . rest)
  (eo 'svg
      '(xmlns . "http://www.w3.org/2000/svg")
      '(xmlns:xlink . "http://www.w3.org/1999/xlink")
      '(version . "1.2")
      `(width . ,(ly:format "~2fmm" (first rest)))
      `(height . ,(ly:format "~2fmm" (second rest)))
      `(viewBox . ,(ly:format "~4f ~4f ~4f ~4f"
			      (third rest) (fourth rest)
			      (fifth rest) (sixth rest)))))

(define (svg-end)
  (ec 'svg))

(define (svg-define-font font font-name scaling)
  (let* ((file-name (if (list? font) (pango-pf-file-name font)
			(ly:font-file-name font)))
	 (lower-name (string-downcase font-name)))
    ;; only embed emmentaler for now
    (if (equal? (substring lower-name 0 (min (string-length lower-name) 10)) "emmentaler")
	(string-append
	 "@font-face {
font-family: '"
	 font-name
	 "';
font-weight: normal;
font-style: normal;
src: url('"
   (string-downcase font-name)
   ".woff');
}
")
	"")))

(define (woff-header paper)
  "TODO:
      * add (ly:version) to font name
      * copy woff font with version alongside svg output
"
  (string-append
   (eo 'defs)
   (eo 'style '(text . "style/css"))
   "<![CDATA[
"
   (define-fonts paper svg-define-font svg-define-font)
   "]]>
"
   (ec 'style)
   (ec 'defs)))

(define (dump-page paper filename page page-number page-count)
  (let* ((outputter (ly:make-paper-outputter (open-file filename "wb") 'svg))
	 (dump (lambda (str) (display str (ly:outputter-port outputter))))
	 (lookup (lambda (x) (ly:output-def-lookup paper x)))
	 (unit-length (lookup 'output-scale))
	 (output-scale (* lily-unit->mm-factor unit-length))
	 (device-width (lookup 'paper-width))
	 (device-height (lookup 'paper-height))
	 (page-width (* output-scale device-width))
	 (page-height (* output-scale device-height)))

    (if (ly:get-option 'svg-woff)
	(module-define! (ly:outputter-module outputter) 'paper paper))
    (dump (svg-begin page-width page-height
		     0 0 device-width device-height))
    (if (ly:get-option 'svg-woff)
	(dump (woff-header paper)))
    (dump (comment (format "Page: ~S/~S" page-number page-count)))
    (ly:outputter-output-scheme outputter
				`(begin (set! lily-unit-length ,unit-length)
					""))
    (ly:outputter-dump-stencil outputter page)
    (dump (svg-end))
    (ly:outputter-close outputter)))

(define (dump-preview paper stencil filename)
  (let* ((outputter (ly:make-paper-outputter (open-file filename "wb") 'svg))
	 (dump (lambda (str) (display str (ly:outputter-port outputter))))
	 (lookup (lambda (x) (ly:output-def-lookup paper x)))
	 (unit-length (lookup 'output-scale))
	 (x-extent (ly:stencil-extent stencil X))
	 (y-extent (ly:stencil-extent stencil Y))
	 (left-x (car x-extent))
	 (top-y (cdr y-extent))
	 (device-width (interval-length x-extent))
	 (device-height (interval-length y-extent))
	 (output-scale (* lily-unit->mm-factor unit-length))
	 (svg-width (* output-scale device-width))
	 (svg-height (* output-scale device-height)))

    (if (ly:get-option 'svg-woff)
	(module-define! (ly:outputter-module outputter) 'paper paper))
    (dump (svg-begin svg-width svg-height
		     left-x (- top-y) device-width device-height))
    (if (ly:get-option svg-woff)
	(dump (woff-header paper)))
    (ly:outputter-output-scheme outputter
				`(begin (set! lily-unit-length ,unit-length)
					""))
    (ly:outputter-dump-stencil outputter stencil)
    (dump (svg-end))
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

(define (output-preview-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
	 (systems (relevant-book-systems book))
	 (to-dump-systems (relevant-dump-systems systems)))
    (dump-preview paper
		  (stack-stencils Y DOWN 0.0
				  (map paper-system-stencil
				       (reverse to-dump-systems)))
		  (format "~a.preview.svg" basename))))
