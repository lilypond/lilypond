;;;; framework-gnome.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-gnome))

(use-modules
 (guile)
 (lily)
 (scm output-gnome))

;; dump?
(define (dump-page outputter page page-number page-count)
  (ly:outputter-dump-stencil outputter (ly:page-stencil page)))

(define-public (output-framework-gnome outputter book scopes fields basename)
  (let* ((bookpaper (ly:paper-book-book-paper book))
	 (pages (ly:paper-book-pages book))
	 (page-number 0)
	 (page-count (length pages)))

    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (dump-page outputter page page-number page-count))
     pages)))





