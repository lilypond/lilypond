;;;; framework-socket.scm
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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


(define-module (scm framework-socket)
  #:export (output-framework)
  )

(use-modules (ice-9 regex)
             (ice-9 string-fun)
             (scm paper-system)
             (ice-9 format)
             (guile)
             (srfi srfi-1)
             (ice-9 pretty-print)
             (srfi srfi-13)
             (lily))

(define (get-page-dimensions paper)
  (let* ((landscape (ly:output-def-lookup paper 'landscape))
         (output-scale (ly:output-def-lookup paper 'output-scale))
         (paper-width (ly:output-def-lookup paper 'paper-width))
         (paper-height (ly:output-def-lookup paper 'paper-height))
         (indent (ly:output-def-lookup paper 'indent))
         (line-width (ly:output-def-lookup paper 'line-width))
         (plain-left-margin (ly:output-def-lookup paper 'left-margin))
         (top-margin (ly:output-def-lookup paper 'top-margin))
         (w (if landscape paper-height paper-width))
         (h (if landscape paper-width paper-height))
         (left-margin (if (null? plain-left-margin)
                          (/ (- w line-width) 2)
                          plain-left-margin))
         ;;      (list w h left-margin top-margin indent line-width)))
         ;;      (convert (lambda (x) (* x output-scale (/ (ly:bp 1))))))
         (unit-length (ly:output-def-lookup paper 'output-scale))
         (convert (lambda (x) (* x lily-unit->mm-factor unit-length))))
    (map convert (list w h left-margin top-margin indent line-width))))

(define-public (output-framework channel book scopes fields)
  (let* ((ctor-arg (if (string? channel)
                       (open-output-file (format #f "~a.socket" channel))
                       channel))
         (outputter (ly:make-paper-outputter
                     ctor-arg
                     'socket))
         (systems (ly:paper-book-systems book))
         (paper (ly:paper-book-paper book))
         (pages (ly:paper-book-pages book)))
    (for-each (lambda (x)
                (let* ((system-stencil (paper-system-stencil x))
                       (x-extent (ly:stencil-extent system-stencil X))
                       (y-extent (ly:stencil-extent system-stencil Y)))
                  (display (ly:format "system ~4l ~4l ~4l ~4l\n"
                                      (car x-extent) (car y-extent) (cdr x-extent) (cdr y-extent)) ctor-arg)
                  (ly:outputter-dump-stencil outputter system-stencil)))
              systems)))

(define-public (output-classic-framework channel book scopes fields)
  (let* ((ctor-arg (if (string? channel)
                       (open-output-file (format #f "~a.socket" channel))
                       channel))
         (outputter (ly:make-paper-outputter
                     ctor-arg
                     'socket))
         (systems (ly:paper-book-systems book))
         (paper (ly:paper-book-paper book)))
    (display (ly:format "paper ~4l\n" (get-page-dimensions paper)) ctor-arg)
    (for-each (lambda (x)
                (let* ((system-stencil (paper-system-stencil x))
                       (x-extent (ly:stencil-extent system-stencil X))
                       (y-extent (ly:stencil-extent system-stencil Y)))
                  (display (ly:format "system ~4l ~4l ~4l ~4l\n"
                                      (car x-extent) (car y-extent) (cdr x-extent) (cdr y-extent)) ctor-arg)
                  (ly:outputter-dump-stencil outputter system-stencil)))
              systems)))

(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
