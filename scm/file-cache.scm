;;;; file-cache.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>


(define cache-hash-tab  (make-hash-table 11))
(define-public (cached-file-contents filename)
  (let*
      ((contents (hash-ref cache-hash-tab filename #f)))

    (if (not (string? contents))
	(begin
	  (set! contents (ly:gulp-file filename))
	  (hash-set! cache-hash-tab filename contents)))
    contents))
