;;;; framework-gnome.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-gnome))

(use-modules
 (guile)
 (lily))

;; dump?
(define (dump-page outputter page page-number page-count)
  (ly:outputter-dump-stencil outputter page))

(define-public (output-framework-gnome outputter book scopes fields basename)
  (let* ((bookpaper (ly:paper-book-book-paper book))
	 (pages (list->vector (map ly:page-stencil
				   (ly:paper-book-pages book)))))
    
    (ly:outputter-dump-stencil
     outputter
     (ly:make-stencil (list 'main outputter pages) '(0 . 0) '(0 . 0)))))

