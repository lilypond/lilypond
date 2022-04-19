;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (lily ps-to-png))

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

(define-public (make-ps-images base-name ps-tmp-name is-eps . rest)
  (let-keywords*
   rest #f
   ((resolution 90)
    (page-width  100)
    (page-height 100)
    (bbox #f)
    (rename-page-1 #f)
    (be-verbose (ly:get-option 'verbose))
    (pixmap-format 'png16m)
    (anti-alias-factor 1)
    (png-width 0)
    (png-height 0))

   (let* ((format-str (format #f "~a" pixmap-format))
          (extension (cond
                      ((string-contains format-str "png") "png")
                      ((string-contains format-str "jpg") "jpeg")
                      ((string-contains format-str "jpeg") "jpeg")
                      (else
                       (ly:error (G_ "Unknown pixmap format ~a") pixmap-format))))
          (page-count (if is-eps
                          1
                          (ps-page-count ps-tmp-name)))
          (multi-page? (> page-count 1))

          (tmp-name (string-append
                     ;; Create the file in the same directory as the
                     ;; destination, or renaming won't work across
                     ;; filesystems, but ...
                     (dirname base-name)
                     "/"
                     ;; make sure there are no special characters in
                     ;; the filename to avoid problems with Ghostscript
                     ;; on Windows.
                     (basename ps-tmp-name)))
          ;; Escape `%' (except `page%d') for ghostscript
          (base-name-gs (string-join
                         (string-split tmp-name #\%)
                         "%%"))
          (pngn-gs (format #f "~a-page%d.~a" base-name-gs extension))

          (fit-page (or (> png-width 0) (> png-height 0)))

          (bbox-width (if is-eps
                          (- (list-ref bbox 2) (list-ref bbox 0))
                          1))
          (bbox-height (if is-eps
                           (- (list-ref bbox 3) (list-ref bbox 1))
                           1))

          ;; If either `png-width` or `png-height` is missing (i.e.,
          ;; zero or negative), use the bbox aspect ratio to derive
          ;; the value from the other dimension.
          (png-width' (if (and fit-page (<= png-width 0))
                          (* png-height (/ bbox-width bbox-height))
                          png-width))
          (png-height' (if (and fit-page (<= png-height 0))
                           (* png-width (/ bbox-height bbox-width))
                           png-height))

          (width (if fit-page
                     png-width'
                     page-width))
          (height (if fit-page
                      png-height'
                      page-height))

          (hw-resolution (* anti-alias-factor resolution))

          (run-strings
           (filter
            string?
            (list
             (format #f "mark /OutputFile (~a)" pngn-gs)
             "/GraphicsAlphaBits 4 /TextAlphaBits 4"
             (if fit-page
                 ;; Get available resolution and magnify it according
                 ;; to `anti-alias-factor`.
                 (format #f "/HWResolution [ \
currentpagedevice /HWResolution get 0 get ~a mul \
currentpagedevice /HWResolution get 1 get ~a mul \
]"
                         anti-alias-factor anti-alias-factor)
                 (format #f "/HWResolution [~a ~a]"
                         hw-resolution hw-resolution))
             (format #f "/DownScaleFactor ~a" anti-alias-factor)
             (if (or (not is-eps) fit-page)
                 (format #f "/PageSize [~a ~a]" width height))
             ;; We use `findprotodevice` because `finddevice` always returns
             ;; the same device instance and we can't reset the page number of
             ;; the device. `findprotodevice copydevice` creates a new device
             ;; instance each time, which can reset the page number.
             (format #f "(~a) findprotodevice copydevice" pixmap-format)
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
             (format #f "/PermitFileWriting (~a*) .addcontrolpath"
                     base-name-gs)
             "} if"
             (gs-safe-run ps-tmp-name)))))

     ;; This is a ghostscript constraint.
     (if (not (and (integer? anti-alias-factor)
                   (<= 1 anti-alias-factor 8)))
         (ly:error (G_ "`anti-alias-factor' must be a positive integer <= 8")))

     ((if (ly:get-option 'gs-api)
          ly:gs-api ly:gs-cli)
      (gs-cmd-args is-eps fit-page) (string-join run-strings " "))

     (map (lambda (n)
            (let*
                ((src (format #f "~a-page~a.~a" tmp-name (1+ n) extension))
                 (dst
                  (if (or (not multi-page?) (and multi-page? rename-page-1))
                      (format #f "~a.~a" base-name extension)
                      (format #f "~a-page~a.~a" base-name (1+ n) extension))))
              (ly:rename-file src dst)
              dst))
          (iota page-count))
     )))
