;;;; framework-gnome.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-gnome)
  :use-module (oop goops)
  #:export (<gnome-outputter>))

;;(define this-module (current-module))

(use-modules (guile) (oop goops) (lily))

(use-modules
 (gnome gtk)
 (gnome gtk gdk-event)
 ;;
 (scm output-gnome)
 )
 
;; the name of the module will change to canvas rsn
(if (resolve-module '(gnome gw canvas))
    (use-modules (gnome gw canvas))
    (use-modules (gnome gw libgnomecanvas)))


(define-public (output-framework-gnome outputter book scopes fields basename)
    ;;(gnome-main book))))
    (ly:outputter-dump-stencil
     outputter
     (ly:make-stencil (list 'gnome-main book) '(0 . 0) '(0 . 0))))
