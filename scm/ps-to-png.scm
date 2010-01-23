;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2010 Jan Nieuwenhuizen <janneke@gnu.org>
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

;; gettext wrapper for guile < 1.7.2
(if (defined? 'gettext)
    (define-public _ gettext)
    (define-public (_ x) x))

(define PLATFORM
  (string->symbol
   (string-downcase
    (car (string-tokenize (vector-ref (uname) 0) char-set:letter)))))

(define (re-sub re sub string)
  (regexp-substitute/global #f re string 'pre sub 'post))

(define (search-executable names)
  (define (helper path lst)
    (if (null? (cdr lst))
	(car lst)
	(if (search-path path (car lst)) (car lst)
	    (helper path (cdr lst)))))

  (let ((path (parse-path (getenv "PATH"))))
    (helper path names)))

(define (search-gs)
  (search-executable '("gs-nox" "gs-8.15" "gs")))

(define (gulp-port port max-length)
  (let ((str (make-string max-length)))
    (read-string!/partial str port 0 max-length)
    str))

(define-public (gulp-file file-name . max-size)
  (ly:gulp-file file-name (if (pair? max-size) (car max-size))))

;; copy of ly:system. ly:* not available via lilypond-ps2png.scm
(define (my-system be-verbose exit-on-error cmd)
  (define status 0)
  (if be-verbose
      (begin
	(format (current-error-port) (_ "Invoking `~a'...") cmd)
	(newline (current-error-port))))
  (set! status (system cmd))
  (if (not (= status 0))
      (begin
	(format (current-error-port)
		(format #f (_ "~a exited with status: ~S") "GS" status))
	(if exit-on-error (exit 1))))
  status)

(define (scale-down-image be-verbose factor file)
  (define (with-pbm)
    (let* ((status 0)
	   (old (string-append file ".old")))
      
      (rename-file file old)
      (my-system
       be-verbose #t
       (format #f
	       "pngtopnm ~a | pnmscale -reduce ~a 2>/dev/null | pnmtopng -compression 9 2>/dev/null > ~a"
	       old factor file))
      (delete-file old)))

  (with-pbm))

(define-public (ps-page-count ps-name)
  (let* ((byte-count 10240)
	 (header (gulp-file ps-name byte-count))
	 (first-null (string-index header #\nul))
	 (match (string-match "%%Pages: ([0-9]+)"
			      (if (number? first-null)
				  (substring header 0 first-null)
				  header))))
    (if match (string->number (match:substring match 1)) 0)))

(define-public (make-ps-images ps-name . rest)
  (let-keywords*
   rest #f
   ((resolution 90)
    (page-width  100)
    (page-height 100)
    (rename-page-1 #f)
    (be-verbose (ly:get-option 'verbose))
    (pixmap-format 'png16m)
    (anti-alias-factor 1))

   (let* ((format-str (format "~a" pixmap-format))
	  (extension (cond
		      ((string-contains format-str "png") "png")
		      ((string-contains format-str "jpg") "jpeg")
		      ((string-contains format-str "jpeg") "jpeg")
		      (else
		       (ly:error "Unknown pixmap format ~a" pixmap-format))))
	  (base (dir-basename ps-name ".ps" ".eps"))
	  (png1 (format "~a.~a" base extension))
	  (pngn (format  "~a-page%d.~a" base extension))
	  (page-count (ps-page-count ps-name))
	  (multi-page? (> page-count 1))
	  (output-file (if multi-page? pngn png1))

	  (gs-variable-options
	   (if multi-page?
	       (format #f "-dDEVICEWIDTHPOINTS=~,2f -dDEVICEHEIGHTPOINTS=~,2f"
		       page-width page-height)
	       "-dEPSCrop"))
	  (cmd (ly:format "~a\
 ~a\
 ~a\
 -dGraphicsAlphaBits=4\
 -dTextAlphaBits=4\
 -dNOPAUSE\
 -sDEVICE=~a\
 -sOutputFile=~S\
 -r~a\
 ~S\
 -c quit"
		       (search-gs)
		       (if be-verbose "" "-q")
		       gs-variable-options
		       pixmap-format
		       output-file 
		       (* anti-alias-factor resolution) ps-name))
	  (status 0)
	  (files '()))

     ;; The wrapper on windows cannot handle `=' signs,
     ;; gs has a workaround with #.
     (if (eq? PLATFORM 'windows)
	 (begin
	   (set! cmd (re-sub "=" "#" cmd))
	   (set! cmd (re-sub "-dSAFER " "" cmd))))

     (set! status (my-system be-verbose #f cmd))

     (set! files
	   (if multi-page?
	       (map
		(lambda (n)
		  (format "~a-page~a.png" base (1+ n)))
		(iota page-count))
	       (list (format "~a.png" base))))
     
     (if (not (= 0 status))
	 (begin
	   (map delete-file files)
	   (exit 1)))

     (if (and rename-page-1 multi-page?)
	 (begin
	   (rename-file (re-sub "%d" "1" pngn) png1)
	   (set! files
		 (cons png1
		       (cdr files)))
	   ))

     (if (not (= 1 anti-alias-factor))
	 (for-each
	  (lambda (f) (scale-down-image be-verbose anti-alias-factor f)) files))
     files)))
