;;;; framework-gnome.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; See output-gnome.scm for usage information.


(define-module (scm framework-gnome))

(use-modules (guile) (oop goops) (lily))

(use-modules
 (srfi srfi-2)
 (ice-9 regex)
 (gnome gtk)
 (gnome gtk gdk-event)
 (gnome gw canvas))

(define-public (output-framework outputter book scopes fields basename)
  (gnome-main book basename))

(define SCROLLBAR-SIZE 20)
(define BUTTON-HEIGHT 25)
(define PANELS-HEIGHT 80)

(define PIXELS-PER-UNIT 2)
(define OUTPUT-SCALE (* 2.5 PIXELS-PER-UNIT))
(define-public output-scale OUTPUT-SCALE)

(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

(define (debugf string . rest)
  (if #f
      (stderr (cons string rest))))
      
(define-class <gnome-outputter> ()
  (name #:init-value "untitled" #:init-keyword #:name #:accessor name)
  (page-stencils ;;#:init-value '#()
   #:init-keyword #:page-stencils #:accessor page-stencils)
  (window #:init-value (make <gtk-window> #:type 'toplevel) #:accessor window)
  (scrolled #:init-value (make <gtk-scrolled-window>) #:accessor scrolled)
  (canvas #:init-value #f #:accessor canvas)
  (page-number #:init-value 0 #:accessor page-number)
  (pixels-per-unit #:init-value PIXELS-PER-UNIT #:accessor pixels-per-unit)
  (text-items #:init-value '() #:accessor text-items)
  (grob #:init-value #f #:accessor grob)
  (item-grobs #:init-value (make-hash-table 31) #:accessor item-grobs)
  (grob-tweaks #:init-value (make-hash-table 31) #:accessor grob-tweaks)
  (window-width #:init-keyword #:window-width #:accessor window-width)
  (window-height #:init-keyword #:window-height #:accessor window-height)
  (canvas-width #:init-keyword #:canvas-width #:accessor canvas-width)
  (canvas-height #:init-keyword #:canvas-height #:accessor canvas-height))

(define-method (initialize (go <gnome-outputter>))
  (let* ((button (make <gtk-button> #:label "Exit"))
	 (next (make <gtk-button> #:label "Next"))
	 (prev (make <gtk-button> #:label "Previous"))
	 (vbox (make <gtk-vbox> #:homogeneous #f))
	 (hbox (make <gtk-hbox> #:homogeneous #f)))

    (set-size-request (window go) (window-width go) (window-height go))
    
    (set-size-request (scrolled go) (window-width go) (- (window-height go)
							 BUTTON-HEIGHT
							 SCROLLBAR-SIZE))

    (new-canvas go)

    (add (window go) vbox)
    (add vbox (scrolled go))
    
    (add (scrolled go) (canvas go))

    ;; buttons
    (add vbox hbox)
    (set-size-request hbox (window-width go) BUTTON-HEIGHT)

    ;; hmm?  These are broken when using <gnome-outputter>.
    ;;(set-child-packing vbox hbox #f #f 0 'end)
    ;;(set-child-packing hbox button #f #f 0 'end)
    
    (set-size-request button (quotient (window-width go) 2) BUTTON-HEIGHT)

    
    (add hbox next)
    (add hbox prev)
    (add hbox button)

    ;; signals
    (connect button 'clicked (lambda (b) (save-tweaks go) (gtk-main-quit)))
    (connect next 'clicked (lambda (b) (dump-page go (1+ (page-number go)))))
    (connect prev 'clicked (lambda (b) (dump-page go (1- (page-number go)))))
    (connect (window go) 'key-press-event
	     (lambda (w e) (key-press-event go w e)))
    
    (show-all (window go))))


(define (gnome-main book name)
  (let* ((paper (ly:paper-book-paper book))
	 (hsize (ly:output-def-lookup paper 'hsize))
	 (vsize (ly:output-def-lookup paper 'vsize))
	 (page-width (inexact->exact (ceiling (* OUTPUT-SCALE hsize))))
	 (page-height (inexact->exact (ceiling (* OUTPUT-SCALE vsize))))
	 ;;(page-width (inexact->exact (ceiling hsize)))
	 ;;(page-height (inexact->exact (ceiling vsize)))

	 (screen-width (gdk-screen-width))
	 (screen-height (gdk-screen-height))
         (desktop-height (- screen-height PANELS-HEIGHT))

	 (go (make <gnome-outputter>
	       #:name name
	       #:page-stencils (list->vector (ly:paper-book-pages book))
	       #:canvas-width page-width
	       #:canvas-height page-height
	       #:window-width
	       ;; huh, *2 -- pixels-per-unit?
	       (min (+ SCROLLBAR-SIZE (* page-width 2)) screen-width)
	       #:window-height
	       (min (+ BUTTON-HEIGHT SCROLLBAR-SIZE (* page-height 2))
		    desktop-height))))

    ;; ugh.  The GOOPS doc promises this is called automagically.
    ;; possibly a goops 1.6.4 problem
    (initialize go)

    (map ly:pango-add-afm-decoder
	 '("lilypond-feta"
	   "lilypond-braces"
	   "lilypond-dyn"
	   "lilypond-parmesan"))

    (dump-page go 0)

    (gtk-main)))

(define (dump-page go number)
  (if (or (not (page-stencils go))
	  (< number 0)
	  (>= number (vector-length (page-stencils go))))
      (stderr "No such page: ~S\n" (1+ number))
      
      (let ((old-canvas (canvas go)))
	(new-canvas go)
	(set! (page-number go) number)
	
	;; no destroy method for gnome-canvas-text yet.
	;;(map destroy (gtk-container-get-children main-canvas))
	;;(map destroy text-items)

	(set! (text-items go) '())
	(debugf "page-stencil ~S: ~S\n"
		(page-number go)		
		(vector-ref (page-stencils go) (page-number go)))
	
	(ly:interpret-stencil-expression 
	 ;; ;;(vector-ref (page-stencils go) (page-number go))
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
  (let* ((file-name (car location))
	 (line (cadr location))
	 (column (caddr location))
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
    
    (debugf "spawning: ~s\n" command)
    (if (= (primitive-fork) 0)
	(let ((command-list (string-split command #\ )));; (get-ifs))))
	  (apply execlp command-list)
	  (primitive-exit)))))
	  
(define location-callback spawn-editor)

(define (get-location grob)
  (and-let* ((p? (procedure? point-and-click))
	     (g grob)
	     (cause (ly:grob-property grob 'cause))
	     (music-origin (if (ly:music? cause)
			       (ly:music-property cause 'origin)
			       ;; How come #<unspecied> [and '()]
			       ;; are #t? :-(
			       #f)))
	    (if (ly:input-location? music-origin)
		(ly:input-location music-origin)
		#f)))

(define-method (tweak (go <gnome-outputter>) item offset)
  (let* ((grob (hashq-ref (item-grobs go) item #f))
	 (origin (hashq-ref (grob-tweaks go) grob '(0 . 0))))
    (if grob
	(hashq-set! (grob-tweaks go) grob (cons (+ (car origin) (car offset))
						(+ (cdr origin) (cdr offset)))))
    (move item (car offset) (cdr offset))))

(define-method (save-tweaks (go <gnome-outputter>))
  (let ;;((file (current-error-port)))
      ((file (open-file (string-append (name go) ".twy") "w")))
    (format file "%% TWEAKS %%\n")
    (hash-fold
     (lambda (key value seed)
       (format file "~S:~S\n"
	       (if (ly:grob? key) (ly:grob-id key) "unidentified grob") value))
     #f (grob-tweaks go))))

;;;(define (item-event go grob item event)
(define (item-event go item event)
  (case (gdk-event:type event)
    ((enter-notify) (gobject-set-property item 'fill-color "red"))
    ((leave-notify) (gobject-set-property item 'fill-color "black"))
    ((button-press)
     (let ((button (gdk-event-button:button event)))
       (cond
	((= button 1)
	 (and-let* ((grob (hashq-ref (item-grobs go) item #f))
		    (location (get-location grob)))
		   (location-callback location)))
	((= button 2)

	 (and-let*
	  ((grob (hashq-ref (item-grobs go) item #f)))
	  
	  (let ((properties (ly:grob-properties grob))
		(basic-properties (ly:grob-basic-properties grob))
		(id (ly:grob-id grob))
		(x (inexact->exact (gdk-event-button:x-root event)))
		(y (inexact->exact (gdk-event-button:y-root event))))
	       
	    (debugf "GROB: ~S\n" grob)
	    (debugf "PROPERTIES: ~S\n" properties)
	    (debugf "BASIC PROPERTIES: ~S\n" basic-properties)
	    
	    (let ((window (make <gtk-window>))
		  (vbox (make <gtk-vbox>))
		  (button (make <gtk-button> #:label "Ok")))
	      
	      (add window vbox)
	      (connect button 'clicked (lambda (b) (destroy window)))

	      (for-each
	       (lambda (x)
		 (let ((button (make <gtk-button>
				 #:xalign 0.0
				 #:label
				 (string-append
				  (symbol->string (car x))
				  ": "
				  (format #f "~S" (cdr x))))))
		   (set-size-request button 150 BUTTON-HEIGHT)
		   (add vbox button)))
	       (cons (list id) properties))
	      (add vbox button)
	      
	      ;; FIXME: how to do window placement?
	      ;; - no effect:
	      (move window x y)
	      (show-all window)
	      ;; - shows actual movement:
	      (move window x y)
	      )))))))
    
    ((2button-press) (gobject-set-property item 'fill-color "green"))
    ((key-press)
     (let ((keyval (gdk-event-key:keyval event))
	   (mods (gdk-event-key:modifiers event))
	   (step (quotient (pixels-per-unit go) 2)))
       (cond ((and (null? mods)
		   (eq? keyval gdk:Up))
	      (tweak go item (cons 0 (- 0 step))))
	     ((and (null? mods)
		   (eq? keyval gdk:Down))
	      (tweak go item (cons 0 step)))
	     ((and (null? mods)
		   (eq? keyval gdk:Left))
	      (tweak go item (cons (- 0 step) 0)))
	     ((and (null? mods)
		   (eq? keyval gdk:Right))
	      (tweak go item (cons step 0)))))))
  #t)

(define (scale-canvas go factor)
  (set! (pixels-per-unit go) (* (pixels-per-unit go) factor))
  (set-pixels-per-unit (canvas go) (pixels-per-unit go))
  (for-each
   (lambda (x)
     (let ((scale (gobject-get-property x 'scale))
	   (points (gobject-get-property x 'size-points)))
       ;;(gobject-set-property x 'scale pixels-per-unit)
       (gobject-set-property x 'size-points (* points factor))))
     (text-items go)))

(define (key-press-event go item event)
  (let ((keyval (gdk-event-key:keyval event))
	(mods (gdk-event-key:modifiers event)))
    (cond ((and (or (eq? keyval gdk:q)
		    (eq? keyval gdk:w))
		(equal? mods '(control-mask modifier-mask)))
	   (gtk-main-quit))
	  ((and #t ;;(null? mods)
		(eq? keyval gdk:plus))
	   (scale-canvas go 2))
	  ((and #t ;; (null? mods)
		(eq? keyval gdk:minus))
	   (scale-canvas go 0.5))
	  ((or (eq? keyval gdk:Page-Up)
	       (eq? keyval gdk:BackSpace))
	   (dump-page go (1- (page-number go))))
	  ((or (eq? keyval gdk:Page-Down)
	       (eq? keyval gdk:space))
	   (dump-page go (1+ (page-number go)))))
    #f))

(define (new-canvas go)
  (set! (canvas go) (make <gnome-canvas>))
  (set-size-request (canvas go) (window-width go) (window-height go))
  (set-scroll-region (canvas go) 0 0 (canvas-width go) (canvas-height go))
  (set-pixels-per-unit (canvas go) (pixels-per-unit go))
  (make <gnome-canvas-rect>
    #:parent (root (canvas go))
    #:x2 (canvas-width go) #:y2 (canvas-height go)
    #:fill-color "white"))

(define output-gnome-module #f)
(define (get-output-gnome-module go)
  (if (not output-gnome-module)
      (let ((m  (resolve-module '(scm output-gnome))))
	(module-define! m 'canvas-root (lambda () (root (canvas go))))
	(module-define! m 'output-scale output-scale)
	(set! output-gnome-module m)))
  output-gnome-module)

(define-public (gnome-output-expression go expr)
  (let* ((m (get-output-gnome-module go))
	 (result (eval expr m)))
    (cond
     ((ly:grob? result) (set! (grob go) result))
     ((is-a? result <gnome-canvas-item>)
      
      ;; AAARGH; grobs happen after stencils
      ;; (connect result 'event (lambda (w e) (item-event go (grob go) w e)))
      (connect result 'event (lambda (w e) (item-event go w e)))
      (if (grob go)
	  (hashq-set! (item-grobs go) result (grob go)))
      (set! (grob go) #f)
      
      (if (is-a? result <gnome-canvas-text>)
	  (set! (text-items go) (cons result (text-items go))))))))

