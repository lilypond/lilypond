;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (lily framework-svg))

(use-modules
 (guile)
 (lily)
 (lily page)
 (lily paper-system)
 (lily output-svg)
 (lily clip-region)
 (srfi srfi-1)
 (srfi srfi-2)
 (srfi srfi-13)
 (ice-9 regex))

(define (svg-begin . rest)
  (string-append
   (eo 'svg #t
       '(xmlns . "http://www.w3.org/2000/svg")
       '(xmlns:xlink . "http://www.w3.org/1999/xlink")
       '(version . "1.2")
       `(width . ,(ly:format "~2fmm" (first rest)))
       `(height . ,(ly:format "~2fmm" (second rest)))
       `(viewBox . ,(ly:format "~4f ~4f ~4f ~4f"
                               (third rest) (fourth rest)
                               (fifth rest) (sixth rest))))
   (eo 'style #t '(type . "text/css"))
   "<![CDATA[
tspan { white-space: pre; }
"))

(define (svg-end)
  (ec 'svg))

(define (mkdirs dir-name mode)
  (let loop ((dir-name (string-split dir-name #\/)) (root ""))
    (if (pair? dir-name)
        (let ((dir (string-append root (car dir-name))))
          (if (not (file-exists? dir))
              (mkdir dir mode))
          (loop (cdr dir-name) (string-append dir "/"))))))

(define output-dir #f)

(define (svg-define-font font font-name scaling)
  (let* ((base-file-name (basename (if (list? font) (pango-pf-file-name font)
                                       (ly:font-file-name font)) ".otf"))
         (woff-file-name (string-regexp-substitute "([.]otf)?$" ".woff"
                                                   base-file-name))
         (woff-file (or (ly:find-file woff-file-name) "/no-such-file.woff"))
         (url (string-append output-dir "/fonts/" (lilypond-version) "/"
                             (basename woff-file-name)))
         (lower-name (string-downcase font-name)))
    (if (file-exists? woff-file)
        (begin
          (if (not (file-exists? url))
              (begin
                (ly:message (G_ "Updating font into: ~a") url)
                (mkdirs (string-append output-dir "/" (dirname url)) #o700)
                (copy-file woff-file url)
                (ly:progress "\n")))
          (format #f
                  "@font-face {
font-family: '~a';
font-weight: normal;
font-style: normal;
src: url('~a');
}
"
                  font-name url))
        "")))

(define (style-defs-end)
  (string-append
   "]]>
"
   (ec 'style)))

(define (woff-header paper dir)
  "TODO:
      * add (ly:version) to font name
      * copy woff font with version alongside svg output
"
  (set! output-dir dir)
  (define-fonts paper svg-define-font svg-define-font))

(define (warn-formats formats)
  (let*
      ((ignored (filter (lambda (f) (not (equal? f "svg"))) formats)))
    (if (pair? ignored)
        (ly:warning (G_ "ignoring unsupported formats ~a") ignored))))

(define (output-stencil basename stencil paper formats)
  (let* ((filename (string-append basename ".svg"))
         (outputter (ly:make-paper-outputter
                     (open-output-file filename #:encoding "UTF-8")
                     stencil-dispatch-alist))
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
         (svg-height (* output-scale device-height))
         (eval-svg (lambda (expr)
                     (ly:outputter-output-scheme outputter expr))))

    (warn-formats formats)
    (if (ly:get-option 'svg-woff)
        (eval-svg `(set-paper ,paper)))
    (dump (svg-begin svg-width svg-height
                     left-x (- top-y) device-width device-height))
    (if (ly:get-option 'svg-woff)
        (eval-svg `(set-paper #f)))
    (if (ly:get-option 'svg-woff)
        (dump (woff-header paper (dirname filename))))
    (dump (style-defs-end))
    (eval-svg `(set-unit-length ,unit-length))
    (ly:outputter-dump-stencil outputter stencil)
    (dump (svg-end))
    (ly:outputter-close outputter)))

(define (output-stencils basename stencils header paper formats)
  (let* ((page-number (1- (ly:output-def-lookup paper 'first-page-number)))
         (page-count (length stencils))
         (filename "")
         (file-suffix (lambda (num)
                        (if (= page-count 1) "" (format #f "-~a" num)))))
    (warn-formats formats)
    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (set! filename (format #f "~a~a"
                              basename
                              (file-suffix page-number)))
       (output-stencil filename page paper '("svg")))
     stencils)))
