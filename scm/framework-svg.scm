;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2020 Jan Nieuwenhuizen <janneke@gnu.org>
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
 (scm clip-region)
 (srfi srfi-1)
 (srfi srfi-2)
 (srfi srfi-13)
 (ice-9 regex))

(define format ergonomic-simple-format)

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
                (ly:message (_ "Updating font into: ~a") url)
                (mkdirs (string-append output-dir "/" (dirname url)) #o700)
                (copy-file woff-file url)
                (ly:progress "\n")))
          (ly:format
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

(define (dump-page paper filename page page-number page-count)
  (let* ((outport (cond-expand
                   (guile-2 (open-output-file filename #:encoding "UTF-8"))
                   (else (open-file filename "wb"))))

         (outputter (ly:make-paper-outputter outport stencil-dispatch-alist))
         (dump (lambda (str) (display str (ly:outputter-port outputter))))
         (lookup (lambda (x) (ly:output-def-lookup paper x)))
         (unit-length (lookup 'output-scale))
         (output-scale (* lily-unit->mm-factor unit-length))
         (device-width (lookup 'paper-width))
         (device-height (lookup 'paper-height))
         (page-width (* output-scale device-width))
         (page-height (* output-scale device-height))
         (eval-svg (lambda (expr)
                     (ly:outputter-output-scheme outputter expr))))

    (if (ly:get-option 'svg-woff)
        (eval-svg `(set-paper ,paper)))
    (dump (svg-begin page-width page-height
                     0 0 device-width device-height))
    (if (ly:get-option 'svg-woff)
        (eval-svg `(set-paper #f)))
    (if (ly:get-option 'svg-woff)
        (dump (woff-header paper (dirname filename))))
    (dump (style-defs-end))
    (dump (comment (format #f "Page: ~S/~S" page-number page-count)))
    (eval-svg `(set-unit-length ,unit-length))
    (ly:outputter-dump-stencil outputter page)
    (dump (svg-end))
    (ly:outputter-close outputter)))

(define (dump-preview paper stencil filename)
  (let* ((outputter (ly:make-paper-outputter
                     (cond-expand
                      (guile-2 (open-output-file filename #:encoding "UTF-8"))
                      (else (open-file filename "wb")))
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

(define (dump-preview-bbox paper stencil filename bbox)
  (let* ((outputter (ly:make-paper-outputter (open-file filename "wb") stencil-dispatch-alist))
         (dump (lambda (str) (display str (ly:outputter-port outputter))))
         (lookup (lambda (x) (ly:output-def-lookup paper x)))
         (unit-length (lookup 'output-scale))
         (output-scale (* lily-unit->mm-factor unit-length))
         (left-x (list-ref bbox 0))
         (top-y (list-ref bbox 1))
         (device-width (list-ref bbox 2))
         (device-height (list-ref bbox 3))
         ;; BUG: Height calculation is off - optional & disabled
         ;; (svg-width (* output-scale device-width))
         ;; (svg-height (* output-scale device-height))
         (eval-svg (lambda (expr)
                     (ly:outputter-output-scheme outputter expr))))

    (if (ly:get-option 'svg-woff)
        (eval-svg `(set-paper ,paper)))
    (dump (svg-begin "" ""
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

(define (clip-systems-to-region basename paper systems region)
  (let* ((extents-system-pairs
          (filtered-map (lambda (paper-system)
                          (let* ((x-ext (system-clipped-x-extent
                                         (paper-system-system-grob paper-system)
                                         region)))
                            (if x-ext
                                (cons x-ext paper-system)
                                #f)))
                        systems))
         (count 0))
    (for-each
     (lambda (ext-system-pair)
       (let* ((xext (car ext-system-pair))
              (paper-system (cdr ext-system-pair))
              (yext (paper-system-extent paper-system Y))
              (bbox (list (car xext) (car yext)
                          (cdr xext) (cdr yext)))
              (paper-system (cdr ext-system-pair))
              (filename (if (< 0 count)
                            (format #f "~a-~a.svg" basename count)
                            (string-append basename ".svg"))))
         (set! count (1+ count))
         (dump-preview-bbox paper
                            (paper-system-stencil paper-system)
                            filename bbox)))
     extents-system-pairs)))

(define (clip-system-SVG basename paper-book)
  (define (clip-score-systems basename systems)
    (let* ((layout (ly:grob-layout (paper-system-system-grob (car systems))))
           (regions (ly:output-def-lookup layout 'clip-regions)))
      (for-each
       (lambda (region)
         (clip-systems-to-region
          (format #f "~a-from-~a-to-~a-clip"
                  basename
                  (rhythmic-location->file-string (car region))
                  (rhythmic-location->file-string (cdr region)))
          layout systems region))
       regions)))

  ;; partition in system lists sharing their layout blocks
  (let* ((systems (ly:paper-book-systems paper-book))
         (count 0)
         (score-system-list '()))
    (fold
     (lambda (system last-system)
       (if (not (and last-system
                     (equal? (paper-system-layout last-system)
                             (paper-system-layout system))))
           (set! score-system-list (cons '() score-system-list)))
       (if (paper-system-layout system)
           (set-car! score-system-list (cons system (car score-system-list))))
       ;; pass value.
       system)
     #f
     systems)
    (for-each (lambda (system-list)
                ;; filter out headers and top-level markup
                (if (pair? system-list)
                    (clip-score-systems
                     (if (> count 0)
                         (format #f "~a-~a" basename count)
                         basename)
                     system-list)))
              score-system-list)))

(define (output-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
         (page-stencils (map page-stencil (ly:paper-book-pages book)))
         (page-number (1- (ly:output-def-lookup paper 'first-page-number)))
         (page-count (length page-stencils))
         (filename "")
         (file-suffix (lambda (num)
                        (if (= page-count 1) "" (format #f "-~a" num)))))
    (if (ly:get-option 'clip-systems) (clip-system-SVG basename book))
    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (set! filename (format #f "~a~a.svg"
                              basename
                              (file-suffix page-number)))
       (dump-page paper filename page page-number page-count))
     page-stencils)))

(define-public (output-classic-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
         (page-stencils (map paper-system-stencil (ly:paper-book-systems book)))
         (page-number (1- (ly:output-def-lookup paper 'first-page-number)))
         (page-count (length page-stencils))
         (filename "")
         (file-suffix (lambda (num)
                        (if (= page-count 1) "" (format #f "-~a" num)))))
    (if (ly:get-option 'clip-systems) (clip-system-SVG basename book))
    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (set! filename (format #f "~a~a.svg"
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
                  (format #f "~a.preview.svg" basename))))

(define (output-crop-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
         (systems (relevant-book-systems book))
         (page-stencils (stack-stencils Y DOWN 0.0
                                        (map paper-system-stencil
                                             (reverse (reverse systems))))))
    (if (ly:get-option 'clip-systems) (clip-system-SVG basename book))
    (dump-preview paper page-stencils (format #f "~a.cropped.svg" basename))))
