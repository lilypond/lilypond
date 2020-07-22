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
          (pngn (format #f "~a-page%d.~a" tmp-name extension))
          (page-count (if is-eps 1 (ps-page-count tmp-name)))
          (multi-page? (> page-count 1))

          ;; Escape `%' (except `page%d') for ghostscript
          (base-name-gs (string-join
                         (string-split tmp-name #\%)
                         "%%"))
          (pngn-gs (format #f "~a-page%d.~a" base-name-gs extension))

          (hw-resolution (* anti-alias-factor resolution))
          (run-strings
           (filter
            string?
            (list
             (ly:format "mark /OutputFile (~a)" pngn-gs)
             "/GraphicsAlphaBits 4 /TextAlphaBits 4"
             (ly:format "/HWResolution [~a ~a]" hw-resolution hw-resolution)
             (ly:format "/DownScaleFactor ~a" anti-alias-factor)
             (if (not is-eps)
                 (ly:format "/PageSize [~a ~a]" page-width page-height))
             ;; We use `findprotodevice` because `finddevice` always returns
             ;; the same device instance and we can't reset the page number of
             ;; the device. `findprotodevice copydevice` creates a new device
             ;; instance each time, which can reset the page number.
             (ly:format "(~a) findprotodevice copydevice" pixmap-format)
             "putdeviceprops setdevice"
             ;; We want to use `selectdevice` instead of `setdevice` because
             ;; `setdevice` doesn't set some defaults. But using `selectdevice`
             ;; can't reset the page number because `selectdevice` uses
             ;; `finddevice` internally. So, as a workaround, we use an
             ;; undocumented `.setdefaultscreen` procedure which is used inside
             ;; `selectdevice` to set the defaults. It works in Ghostscript
             ;; 9.52 but may not work if the internal implementation of
             ;; `selectdevice` is changed in the future.
             "/.setdefaultscreen where {"
             "pop .setdefaultscreen"
             "} {"
             "(Warning: .setdefaultscreen not available) print"
             "} ifelse"
             ;; Enable writing to the formattable OutputFile with a wildcard.
             ;; (When setting -sOutputFile from the command line, this happens
             ;; in gs_main_add_outputfile_control_path.)
             "/.addcontrolpath where { pop"
             (ly:format "/PermitFileWriting (~a*) .addcontrolpath" base-name-gs)
             "} if"
             (gs-safe-run tmp-name)))))

     ((if (ly:get-option 'gs-api)
          ly:gs-api ly:gs-cli)
      (gs-cmd-args is-eps) (string-join run-strings " "))

     (map (lambda (n)
            (let*
                ((src (ly:format "~a-page~a.~a" tmp-name (1+ n) extension))
                 (dst
                  (if (or (not multi-page?) (and multi-page? rename-page-1))
                      (ly:format "~a.~a" base-name extension)
                      (ly:format "~a-page~a.~a" base-name (1+ n) extension))))
              (ly:rename-file src dst)
              dst))
          (iota page-count))
     )))
