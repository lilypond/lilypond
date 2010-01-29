;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2010 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>
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


(use-modules (ice-9 rdelim))

(define standalone (not (defined? 'ly:gulp-file)))
;;(write standalone (current-error-port))

(define (gulp-file name)
  (let* ((file (open-input-file name))
	 (text (read-delimited "" file)))
    (close file)
    text))

(define (scm-gulp-file name)
  (set! %load-path 
	(cons (string-append (getenv "LILYPOND_DATADIR") "/ly")
	      (cons (string-append (getenv "LILYPOND_DATADIR") "/ps")
		    %load-path)))
  (let ((path (%search-load-path name)))
       (if path
	   (gulp-file path)
	   (gulp-file name))))

(define (scm-number->string x)
  (let ((e (inexact->exact x)))
    (string-append (if (= e x)
		       (number->string e)
		       (number->string x))
		   " ")))

(define ly:gulp-file scm-gulp-file)
(define ly:number->string scm-number->string)

(eval-string (ly:gulp-file "lily.scm"))
