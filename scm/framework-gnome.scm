;;;; framework-gnome.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-gnome))
(use-modules (guile) (lily))

(use-modules 
 (gnome gtk)
 (gnome gtk gdk-event))
 
;; the name of the module will change to canvas rsn
(if (resolve-module '(gnome gw canvas))
    (use-modules (gnome gw canvas))
    (use-modules (gnome gw libgnomecanvas)))

(define-public (output-framework-gnome outputter book scopes fields basename)
  (let* ((bookpaper (ly:paper-book-book-paper book))
	 (pages (list->vector (ly:paper-book-pages book))))

    ;; yay, it works
    ;; TODO: goops class instance with
    ;;  - main-window?
    ;;  - main-scrolled window
    ;;  - canvas
    ;;  - page-number
    ;;  - pixels-per-unit (or can get from canvas?)
    ;;  - text-items list
    ;;  - item-locations hashmap
    
    ;; give that as first argument to all outputter/stencil functions?
    ;; 
    (let* ((window (make <gtk-window> #:type 'toplevel)))
      (write window))
    
    (ly:outputter-dump-stencil
     outputter
     (ly:make-stencil (list 'main outputter bookpaper pages)
		      '(0 . 0) '(0 . 0)))))

