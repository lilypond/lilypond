;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2015 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define (search-pngtopam)
  (search-executable
   (if (eq? PLATFORM 'windows)
       '("pngtopam.exe" "pngtopnm.exe")
       '("pngtopam" "pngtopnm"))))

(define (search-pamscale)
  (search-executable
   (if (eq? PLATFORM 'windows)
       '("pamscale.exe" "pnmscale.exe")
       '("pamscale" "pnmscale"))))

(define (search-pnmtopng)
  (search-executable
   (if (eq? PLATFORM 'windows)
       '("pnmtopng.exe")
       '("pnmtopng"))))

(define (scale-down-image factor file)
  (let* ((port-tmp1 (make-tmpfile))
         (tmp1-name (port-filename port-tmp1))
         (port-tmp2 (make-tmpfile))
         (tmp2-name (port-filename port-tmp2))
         ;; Netpbm commands (pngtopnm, pnmscale, pnmtopng)
         ;; outputs only standard output instead of a file.
         ;; So we need pipe and redirection.
         ;; However, ly:system can't handle them.
         ;; Therefore, we use ly:system-with-shell.
         (cmd
          (ly:format
           "~a \"~a\" | ~a -reduce ~a | ~a -compression 9 > \"~a\""
           (search-pngtopam) tmp1-name
           (search-pamscale) factor
           (search-pnmtopng)
           tmp2-name)))

    (close-port port-tmp1)
    (close-port port-tmp2)
    (ly:debug (_ "Copying `~a' to `~a'...") file tmp1-name)
    (copy-binary-file file tmp1-name)
    (ly:system-with-shell cmd)
    (ly:debug (_ "Copying `~a' to `~a'...") tmp2-name file)
    (copy-binary-file tmp2-name file)
    (ly:debug (_ "Deleting `~a'...") tmp1-name)
    (delete-file tmp1-name)
    (ly:debug (_ "Deleting `~a'...") tmp2-name)
    (delete-file tmp2-name)))

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

          (*unspecified* (if #f #f))
          (cmd
           (remove (lambda (x) (eq? x *unspecified*))
                   (list
                    (search-gs)
                    (if (ly:get-option 'verbose) *unspecified* "-q")
                    (if (or (ly:get-option 'gs-load-fonts)
                            (ly:get-option 'gs-load-lily-fonts)
                            (eq? PLATFORM 'windows))
                        "-dNOSAFER"
                        "-dSAFER")

                    (if is-eps
                        "-dEPSCrop"
                        (ly:format "-dDEVICEWIDTHPOINTS=~$" page-width))
                    (if is-eps
                        *unspecified*
                        (ly:format "-dDEVICEHEIGHTPOINTS=~$" page-height))
                    "-dGraphicsAlphaBits=4"
                    "-dTextAlphaBits=4"
                    "-dNOPAUSE"
                    "-dBATCH"
                    (ly:format "-sDEVICE=~a" pixmap-format)
                    "-dAutoRotatePages=/None"
                    "-dPrinted=false"
                    (string-append "-sOutputFile=" output-file)
                    (ly:format "-r~a" (* anti-alias-factor resolution))
                    (string-append "-f" tmp-name))))
          (files '()))

     (ly:system cmd)

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

     (if (not (= 1 anti-alias-factor))
         (for-each
          (lambda (f) (scale-down-image anti-alias-factor f)) files))
     files)))
