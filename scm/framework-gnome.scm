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
;; (scm output-gnome)
 )
 
;; the name of the module will change to canvas rsn
(if (resolve-module '(gnome gw canvas))
    (use-modules (gnome gw canvas))
    (use-modules (gnome gw libgnomecanvas)))

(define SCROLLBAR-SIZE 20)
(define BUTTON-HEIGHT 25)
(define PANELS-HEIGHT 80)

(define PIXELS-PER-UNIT 2)
(define OUTPUT-SCALE (* 2.5 PIXELS-PER-UNIT))

;; helper functions -- sort this out
(define (stderr string . rest)
  ;; debugging
  (if #t
      (begin
	(apply format (cons (current-error-port) (cons string rest)))
	(force-output (current-error-port)))))

(define-class <gnome-outputter> ()
  (page-stencils ;;#:init-value '#()
   #:init-keyword #:page-stencils #:accessor page-stencils)
  (window #:init-value (make <gtk-window> #:type 'toplevel) #:accessor window)
  (scrolled #:init-value (make <gtk-scrolled-window>) #:accessor scrolled)
  (canvas #:init-value #f #:accessor canvas)
  (page-number #:init-value 0 #:accessor page-number)
  (pixels-per-unit #:init-value PIXELS-PER-UNIT #:accessor pixels-per-unit)
  (text-items #:init-value '() #:accessor text-items)
  (location #:init-value #:f #:accessor location)
  (item-locations #:init-value (make-hash-table 31) #:accessor item-locations)
  (window-width #:init-keyword #:window-width #:accessor window-width)
  (window-height #:init-keyword #:window-height #:accessor window-height)
  (canvas-width #:init-keyword #:canvas-width #:accessor canvas-width)
  (canvas-height #:init-keyword #:canvas-height #:accessor canvas-height))

;;(define-method (initialize (go <gnome-outputter>))
;; )

(define (setup go)
  (let* ((button (make <gtk-button> #:label "Exit"))
	 (next (make <gtk-button> #:label "Next"))
	 (prev (make <gtk-button> #:label "Previous"))
	 (vbox (make <gtk-vbox> #:homogeneous #f))
	 (hbox (make <gtk-hbox> #:homogeneous #f)))

    (set-size-request (window go) (window-width go) (window-height go))

    (new-canvas go)

    (add (window go) vbox)
    (add vbox (scrolled go))
    
    (add (scrolled go) (canvas go))

    ;; buttons
    (add vbox hbox)
    (set-size-request hbox (window-width go) BUTTON-HEIGHT)

    ;; hmm?
    ;;(set-child-packing vbox hbox #f #f 0 'end)
    ;;(set-child-packing hbox button #f #f 0 'end)
    
    (set-size-request button (quotient (window-width go) 2) BUTTON-HEIGHT)
    (add hbox next)
    (add hbox prev)
    (add hbox button)

    ;; signals
    (gtype-instance-signal-connect
     button 'clicked (lambda (b) (gtk-main-quit)))
    (gtype-instance-signal-connect
     next 'clicked (lambda (b) (dump-page go (1+ (page-number go)))))
    (gtype-instance-signal-connect
     prev 'clicked (lambda (b) (dump-page go (1- (page-number go)))))
    (gtype-instance-signal-connect
     (window go) 'key-press-event key-press-event)
    
    (show-all (window go))))


(define-public (output-framework-gnome outputter book scopes fields basename)
  (let* ((book-paper (ly:paper-book-book-paper book))
	 
         (hsize (ly:output-def-lookup book-paper 'hsize))
	 (vsize (ly:output-def-lookup book-paper 'vsize))
	 (page-width (inexact->exact (ceiling (* OUTPUT-SCALE hsize))))
	 (page-height (inexact->exact (ceiling (* OUTPUT-SCALE vsize))))
	 ;;(page-width (inexact->exact (ceiling hsize)))
	 ;;(page-height (inexact->exact (ceiling vsize)))

	 (screen-width (gdk-screen-width))
	 (screen-height (gdk-screen-height))
         (desktop-height (- screen-height PANELS-HEIGHT))

	 (go (make <gnome-outputter>
	       #:page-stencils (list->vector (ly:paper-book-pages book))
	       #:canvas-width page-width
	       #:canvas-height page-height
	       #:window-width
	       ;; huh, *2 -- pixels-per-unit?
	       (min (+ SCROLLBAR-SIZE (* page-width 2)) screen-width)
	       #:window-height
	       (min (+ BUTTON-HEIGHT SCROLLBAR-SIZE (* page-height 2))
		    desktop-height))))

    (setup go)
    (dump-page go 0)
    (gtk-main)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gnome stuff  --- move to framework-gnome
;;(define (dump-page (go <gnome-outputter>) number)

(define output-gnome-module
  (make-module 1021 (list (resolve-interface '(scm output-gnome)))))

(define-public (gnome-output-expression go expr)
  (stderr "HI\n")
  (let ((m output-gnome-module))

    ;; this does not seem to work
    (module-define! m 'go go)

    (eval expr m)))



(define (dump-page go number)
  (if (or (not (page-stencils go))
	  (< number 0)
	  (>= number (vector-length (page-stencils go))))
      (stderr "No such page: ~S\n" (1+ number))
      
      (let ((old-canvas (canvas go)))
	(new-canvas go)
	(set! (page-number go) number)
	
	;; no destroy method for gnome-canvas-text?
	;;(map destroy (gtk-container-get-children main-canvas))
	;;(map destroy text-items)

	;;Hmm
	;;(set! main-canvas canvas)
	(set! (text-items go) '())
	;;(ly:outputter-dump-stencil output-canvas
	;;			   (vector-ref page-stencils page-number))
	(stderr "page-stencil ~S: ~S\n"
		(page-number go)		
		(vector-ref (page-stencils go) (page-number go)))
	
	(ly:interpret-stencil-expression 
	 ;;(vector-ref (page-stencils go) (page-number go))
	 (ly:stencil-expr (vector-ref (page-stencils go) (page-number go)))
	 gnome-output-expression go '(0 . 0))
	
	(if old-canvas (destroy old-canvas))
	(add (scrolled go) (canvas go))
	(show (canvas go)))))

(define x-editor #f)
(define (get-x-editor)
  (if (not x-editor)
      (set! x-editor (getenv "XEDITOR")))
  x-editor)

(define ifs #f)
(define (get-ifs)
  (if (not ifs)
      (set! ifs (getenv "IFS")))
  (if (not ifs)
      (set! ifs " 	"))
  ifs)
      
(define (spawn-editor location)
  (let* ((line (car location))
	 (column (cadr location))
	 (file-name (caddr location))
	 (template (substring (get-x-editor) 0))
	 
	 ;; Adhere to %l %c %f?
	 (command
	  (regexp-substitute/global
	   #f "%l" (regexp-substitute/global
		    #f "%c"
		    (regexp-substitute/global
		     #f "%f" template 'pre file-name 'post)
		    'pre (number->string column)
		    'post)
	   'pre (number->string line) 'post)))
    
    (stderr "spawning: ~s\n" command)
    (if (= (primitive-fork) 0)
	(let ((command-list (string-split command #\ )));; (get-ifs))))
	  (apply execlp command-list)
	  (primitive-exit)))))
	  
(define location-callback spawn-editor)

(define (item-event item event . data)
  (case (gdk-event:type event)
    ((enter-notify) (gobject-set-property item 'fill-color "red"))
    ((leave-notify) (gobject-set-property item 'fill-color "black"))
    ((button-press)
     (let ((location (hashq-ref item-locations item #f)))
       (if location
	   (location-callback location)
	   (stderr "no location\n"))))
    ((2button-press) (gobject-set-property item 'fill-color "red")))
  #t)

(define (scale-canvas factor)
  (set! pixels-per-unit (* pixels-per-unit factor))
  (set-pixels-per-unit main-canvas pixels-per-unit)
  (for-each
   (lambda (x)
     (let ((scale (gobject-get-property x 'scale))
	   (points (gobject-get-property x 'size-points)))
       ;;(gobject-set-property x 'scale pixels-per-unit)
       (gobject-set-property x 'size-points (* points factor))))
     text-items))

(define (key-press-event item event . data)
  (let ((keyval (gdk-event-key:keyval event))
	(mods (gdk-event-key:modifiers event)))
    (cond ((and (or (eq? keyval gdk:q)
		    (eq? keyval gdk:w))
		(equal? mods '(control-mask modifier-mask)))
	   (gtk-main-quit))
	  ((and #t ;;(null? mods)
		(eq? keyval gdk:plus))
	   (scale-canvas 2))
	  ((and #t ;; (null? mods)
		(eq? keyval gdk:minus))
	   (scale-canvas 0.5))
	  ((or (eq? keyval gdk:Page-Up)
	       (eq? keyval gdk:BackSpace))
	   (dump-page (1- page-number)))
	  ((or (eq? keyval gdk:Page-Down)
	       (eq? keyval gdk:space))
	   (dump-page (1+ page-number))))
    #f))

;;(define (new-canvas go <gnome-outputter>)
(define (new-canvas go)
  (set! (canvas go) (make <gnome-canvas>))
  (set-size-request (canvas go) (window-width go) (window-height go))
  (set-scroll-region (canvas go) 0 0 (canvas-width go) (canvas-height go))
  (set-pixels-per-unit (canvas go) (pixels-per-unit go))
  (make <gnome-canvas-rect>
    #:parent (root (canvas go))
    #:x2 (canvas-width go) #:y2 (canvas-height go)
    #:fill-color "white"))

