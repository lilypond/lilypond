;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2020 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;          Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define-public (layout-extract-page-properties layout)
  (list (append `((line-width . ,(ly:paper-get-number
                                  layout 'line-width)))
                (ly:output-def-lookup layout 'text-font-defaults))))

;;;;;;;;;;;;;;;;;;

(define ((marked-up-headfoot what-odd what-even)
         layout scopes page-number is-last-bookpart is-bookpart-last-page)
  "Read variables @var{what-odd}, @var{what-even} from @var{layout},
and interpret them as markup.  The @var{props} argument will include
variables set in @var{scopes} and @code{page:is-bookpart-last-page},
@code{page:is-last-bookpart}, @code{page:page-number-string}, and
@code{page:page-number}. Returns a stencil."

  (define (get sym)
    (ly:output-def-lookup layout sym))

  (define (interpret-in-page-env potential-markup)
    (if (markup? potential-markup)
        (let* ((alists (map ly:module->alist scopes))
               (prefixed-alists
                (map (lambda (alist)
                       (map (lambda (entry)
                              (cons
                               (string->symbol
                                (string-append
                                 "header:"
                                 (symbol->string (car entry))))
                               (cdr entry)))
                            alist))
                     alists))
               (number-type (get 'page-number-type))
               (pgnum-alist
                (list
                 (cons 'header:tagline
                       (ly:modules-lookup scopes 'tagline
                                          (ly:output-def-lookup layout 'tagline)))
                 (cons 'page:is-last-bookpart is-last-bookpart)
                 (cons 'page:is-bookpart-last-page is-bookpart-last-page)
                 (cons 'page:page-number-string
                       (number-format number-type page-number))
                 (cons 'page:page-number page-number)))
               (props (append
                       (list pgnum-alist)
                       prefixed-alists
                       (layout-extract-page-properties layout))))
          (interpret-markup layout props potential-markup))

        empty-stencil))

  (interpret-in-page-env
   (if (and (even? page-number)
            (markup? (get what-even)))
       (get what-even)
       (get what-odd))))

(export marked-up-headfoot)

(define ((marked-up-title what) layout scopes)
  "Read variables @var{what} from @var{scopes}, and interpret it as markup.
The @var{props} argument will include variables set in @var{scopes} (prefixed
with `header:'."

  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (markup? x) x #f)))

  (let* ((alists (map ly:module->alist scopes))
         (prefixed-alist
          (map (lambda (alist)
                 (map (lambda (entry)
                        (cons
                         (string->symbol
                          (string-append
                           "header:"
                           (symbol->string (car entry))))
                         (cdr entry)))
                      alist))
               alists))
         (props (append prefixed-alist
                        (layout-extract-page-properties layout)))

         (title-markup (ly:output-def-lookup layout what)))

    (if (markup? title-markup)
        (interpret-markup layout props title-markup)
        empty-stencil)))
(export marked-up-title)
