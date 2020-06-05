;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2020 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (scm ps-to-png))

(use-modules
 (ice-9 optargs)
 (ice-9 regex)
 (ice-9 rw)
 (srfi srfi-1)
 (srfi srfi-13)
 (srfi srfi-14)
 (lily)
 )

(define (re-sub re sub string)
  (regexp-substitute/global #f re string 'pre sub 'post))

(define-public (gulp-file file-name . max-size)
  (ly:gulp-file file-name (if (pair? max-size) (car max-size))))

(define-public (ps-page-count ps-name)
  (let* ((byte-count 10240)
         (header (gulp-file ps-name byte-count))
         (first-null (string-index header #\nul))
         (match (string-match "%%Pages: ([0-9]+)"
                              (if (number? first-null)
                                  (substring header 0 first-null)
                                  header))))
    (if match (string->number (match:substring match 1)) 0)))

(define-public (make-ps-images base-name tmp-name is-eps . rest)
  (let-keywords*
   rest #f
   ((resolution 90)
    (page-width  100)
    (page-height 100)
    (rename-page-1 #f)
    (be-verbose (ly:get-option 'verbose))
    (pixmap-format 'png16m)
    (anti-alias-factor 1))

   (let* ((format-str (format #f "~a" pixmap-format))
          (extension (cond
                      ((string-contains format-str "png") "png")
                      ((string-contains format-str "jpg") "jpeg")
                      ((string-contains format-str "jpeg") "jpeg")
                      (else
                       (ly:error "Unknown pixmap format ~a" pixmap-format))))
          (png1 (format #f "~a.~a" base-name extension))
          (pngn (format #f "~a-page%d.~a" base-name extension))
          (page-count (ps-page-count tmp-name))
          (multi-page? (> page-count 1))

          ;; Escape `%' (except `page%d') for ghostscript
          (base-name-gs (string-join
                         (string-split base-name #\%)
                         "%%"))
          (png1-gs (format #f "~a.~a" base-name-gs extension))
          (pngn-gs (format #f "~a-page%d.~a" base-name-gs extension))
          (output-file (if multi-page? pngn-gs png1-gs))

          (args
           (filter string?
                   (list
                    (search-gs)
                    (if (not (ly:get-option 'verbose)) "-q")
                    "-dNOSAFER"
                    "-dNOPAUSE"
                    "-dNODISPLAY"
                    "-dBATCH"
                    (if is-eps
                        "-dEPSCrop")
                    "-dAutoRotatePages=/None"
                    "-dPrinted=false")))
          (hw-resolution (* anti-alias-factor resolution))

          (run-strings (filter
                        string?
                        (list

                         (ly:format "mark /OutputFile (~a)" output-file)
                         "/GraphicsAlphaBits 4 /TextAlphaBits 4"
                         (ly:format "/HWResolution [~a ~a]" hw-resolution hw-resolution)
                         (ly:format "/DownScaleFactor ~a" anti-alias-factor)
                         (if (not is-eps)
                             (ly:format "/PageSize [~a ~a]" page-width page-height))
                         (ly:format "(~a) findprotodevice copydevice putdeviceprops setdevice" pixmap-format)
                         (ly:format "(~a) ~a run" tmp-name gs-safe-option))))

          (files '()))

     ((if (ly:get-option 'gs-api)
          ly:gs-api ly:gs-cli)
      args (string-join run-strings " "))

     (set! files
           (if multi-page?
               (map
                (lambda (n)
                  (format #f "~a-page~a.png" base-name (1+ n)))
                (iota page-count))
               (list (format #f "~a.png" base-name))))

     (if (and rename-page-1 multi-page?)
         (begin
           (rename-file (re-sub "%d" "1" pngn) png1)
           (set! files
                 (cons png1
                       (cdr files)))
           ))

     files)))
