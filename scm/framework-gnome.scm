;;;; framework-gnome.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm framework-gnome))
(use-modules (guile) (lily))

(define-public (output-framework-gnome outputter book scopes fields basename)
  (let* ((bookpaper (ly:paper-book-book-paper book))
	 (pages (list->vector (ly:paper-book-pages book))))

;; try #1
    (if #f
	(use-modules 
	 (gnome gtk)
	 (gnome gtk gdk-event)
	 (gnome gw canvas)))

;; try #2
    ;; waarom maken ze dit nou allemaal toch weer zo moeilijk?
    ;; is there any documentation about modules for guile 1.6.4?
    (map (lambda (x) (ly:import-module (current-module) (resolve-module x)))
	 '((gnome gtk)
	   (gnome gtk gdk-event)
	   (gnome gw canvas)))

    (if #f
	(let* ((window (make <gtk-window> #:type 'toplevel)))
	  (write window)))
    
    ;; try #3
    (if #f
	(let ((the-module-previously-known-as-current-module (current-module)))
	  (map (lambda (x) (ly:import-module
			    the-module-previously-known-as-current-module
			    (resolve-module x)))
	       '((gnome gtk)
		 (gnome gtk gdk-event)
		 (gnome gw canvas)))
	  
	  (eval '(let* ((window (make <gtk-window> #:type 'toplevel)))
		   (write window))
		the-module-previously-known-as-current-module)))

   ;;try #4
    (if #f
	(eval '(use-modules
		(gnome gtk)
		(gnome gtk gdk-event)
		(gnome gw canvas))
	      (let* ((window (make <gtk-window> #:type 'toplevel)))
		(write window))
	      (current-module)))

    (ly:outputter-dump-stencil
     outputter
     (ly:make-stencil (list 'main outputter bookpaper pages)
		      '(0 . 0) '(0 . 0)))))

