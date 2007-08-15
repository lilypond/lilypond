;;;; framework-gnome.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2007 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; See output-gnome.scm for usage information.


(define-module (scm framework-gnome))

(use-modules (guile)
	     (oop goops)
	     (scm page)
	     (scm paper-system)
	     (lily))

(use-modules
 (srfi srfi-2)
 (ice-9 regex)
 (gnome gtk)
 (gnome gtk gdk-event)
 (gnome gw canvas))

(define-public (output-framework basename book scopes fields )
  (gnome-main book basename))

(define SCROLLBAR-SIZE 20)
(define BUTTON-HEIGHT 25)
(define PANELS-HEIGHT 80)

(define PIXELS-PER-UNIT 2)
;; 2.5??
(define OUTPUT-SCALE (* 2.5 PIXELS-PER-UNIT))
(define-public output-scale OUTPUT-SCALE)

(define (debugf string . rest)
  (if #f
      (apply stderr (cons string rest))))

(define-class <gnome-outputter> ()
  (name #:init-value "untitled" #:init-keyword #:name #:accessor name)

  ;; FIXME
  (dragging #:init-value #f #:accessor dragging)
  (drag-origin #:init-value #f #:accessor drag-origin)
  (drag-location #:init-value #f #:accessor drag-location)
  
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
  (window-width #:init-keyword #:window-width #:accessor window-width)
  (window-height #:init-keyword #:window-height #:accessor window-height)
  (canvas-width #:init-keyword #:canvas-width #:accessor canvas-width)
  (canvas-height #:init-keyword #:canvas-height #:accessor canvas-height))

(define-method (initialize (go <gnome-outputter>))
  (let* ((save (make <gtk-button> #:label "Save"))
	 (exit (make <gtk-button> #:label "Exit"))
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
    
    (set-size-request exit (quotient (window-width go) 2) BUTTON-HEIGHT)

    
    (add hbox next)
    (add hbox prev)
    (add hbox save)
    (add hbox exit)

    ;; signals
    (connect exit 'clicked (lambda (b) (gtk-main-quit)))
    (connect save 'clicked (lambda (b) (save-tweaks go)))
    (connect next 'clicked (lambda (b) (dump-page go (1+ (page-number go)))))
    (connect prev 'clicked (lambda (b) (dump-page go (1- (page-number go)))))
    (connect (window go) 'key-press-event
	     (lambda (w e) (key-press-event go w e)))
    
    (show-all (window go))))


(define (gnome-main book name)
  (let* ((paper (ly:paper-book-paper book))
	 (paper-width (ly:output-def-lookup paper 'paper-width))
	 (paper-height (ly:output-def-lookup paper 'paper-height))
	 (page-width (inexact->exact (ceiling (* OUTPUT-SCALE paper-width))))
	 (page-height (inexact->exact (ceiling (* OUTPUT-SCALE paper-height))))
	 ;;(page-width (inexact->exact (ceiling paper-width)))
	 ;;(page-height (inexact->exact (ceiling paper-height)))

	 (screen-width (gdk-screen-width))
	 (screen-height (gdk-screen-height))
         (desktop-height (- screen-height PANELS-HEIGHT))

	 (go (make <gnome-outputter>
	       #:name name
	       #:page-stencils (list->vector (map page-stencil (ly:paper-book-pages book)))
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
	 (char (caddr location))
	 (column (cadddr location))
	 (command (get-editor-command file line char column)))
    (debugf "spawning: ~s\n" command)
    (if (= (primitive-fork) 0)
	(let ((command-list (string-split command #\ )));; (get-ifs))))
	  (apply execlp command-list)
	  (primitive-exit)))))

(define location-callback spawn-editor)

(define (get-location grob)
  (and-let* ((p (procedure? point-and-click))
	     (g grob)
	     (cause (ly:grob-property grob 'cause))
	     (music-origin (if (ly:event? cause)
			       (ly:event-property cause 'origin)
			       ;; How come #<unspecified> [and '()]
			       ;; are #t? :-(
			       #f)))
	    (if (ly:input-location? music-origin)
		(ly:input-location music-origin)
		#f)))

;; todo: how to integrate nicely?
;(define-public (tweak-grob-property grob sym val)
;  (set! (ly:grob-property grob sym) val))


(define-method (tweak (go <gnome-outputter>) item offset)
  (let* ((grob (hashq-ref (item-grobs go) item #f))
	 (extra-offset (ly:grob-property grob 'extra-offset))
	 (origin (if (null? extra-offset) '(0 . 0)
	 	     (offset-flip-y extra-offset))))

    (if grob
	(ly:grob-replace-tweak
	 grob (list tweak-grob-property
		    'extra-offset
		    (offset-flip-y (offset-add origin offset)))))))

(define-method (save-tweaks (go <gnome-outputter>))
  (let* ((dumper (ly:make-dumper))
	 (tweaks (ly:all-tweaks))
	 (serialized-tweaks
	  (map
	   (lambda (tweak) (append 
			    (list (ly:dumper-key-serial dumper (car tweak))
				  (list 'unquote (procedure-name (cadr tweak))))
			    (cddr tweak)))
	   tweaks)))

    (if (not (null? serialized-tweaks))
	(let ((file (open-file (string-append (name go) ".twy") "w")))
	  (format file
		  ";;;tweaks. Generated file. Do not edit. 
(ly:tweak-clear-registry)
(ly:tweak-define-keys `~S)
(ly:tweak-define-tweaks `~S)"
		  (ly:dumper-definitions dumper)
		  serialized-tweaks)))))

;;;(define (item-event go grob item event)
(define (item-event go item event)
  ;;(stderr "EVENT: ~S\n" event)
  ;;(stderr "TYPE: ~S\n" (gdk-event:type event))
  (case (gdk-event:type event)
    ((enter-notify) (gobject-set-property item 'fill-color "red"))
    ((leave-notify) (gobject-set-property item 'fill-color "black"))
    ((motion-notify) (if (ly:grob? (dragging go))
			 (let ((x (gdk-event-motion:x event))
			       (y (gdk-event-motion:y event))
			       (s output-scale)
			       (r (drag-location go)))
			   ;;(stderr "MOVED AT: ~S ~S\n" x y)
			   (move item (/ (- x (car r)) s) (/ (- y (cdr r)) s))
			   (set! (drag-location go) (cons x y)))))
    ((button-release) (if (ly:grob? (dragging go))
			  (let ((x (gdk-event-button:x event))
				(y (gdk-event-button:y event))
				(s output-scale)
				(o (drag-origin go))
				(r (drag-location go)))
			    (move item (/ (- x (car r)) s) (/ (- y (cdr r)) s))
			    (set! (drag-location go) #f)
			    (set! (drag-origin go) #f)
			    (stderr "RELEASE at: ~S ~S\n" x y)
			    (set! (dragging go) #f)
			    (tweak go item (cons (/ (- x (car o)) s)
						 (/ (- y (cdr o)) s))))))
    ((button-press)
     (let ((button (gdk-event-button:button event)))
       (cond
	((= button 1)
	 (if (null? (gdk-event-button:modifiers event))
	     (let ((x (gdk-event-button:x event))
		   (y (gdk-event-button:y event)))
	       (stderr "CLICK at: ~S ~S\n" x y)
	       (set! (dragging go) (hashq-ref (item-grobs go) item #f))
	       (set! (drag-origin go) (cons x y))
	       (set! (drag-location go) (cons x y)))
	     (begin
	       (stderr "CLICK WITH MODIFIERS: ~S\n"
		       (gdk-event-button:modifiers event))
	       
	       ;; some modifier, do jump to source
	       (and-let* ((grob (hashq-ref (item-grobs go) item #f))
			  (location (get-location grob)))
			 (location-callback location)))))
	((= button 2)
	 (and-let* ((grob (hashq-ref (item-grobs go) item #f)))
		   
		   (let ((properties (ly:grob-properties grob))
			 (basic-properties (ly:grob-basic-properties grob))
			 (x (inexact->exact (gdk-event-button:x-root event)))
			 (y (inexact->exact (gdk-event-button:y-root event))))
		     
		     (debugf "GROB: ~S\n" grob)
		     (debugf "PROPERTIES: ~S\n" properties)
		     (debugf "BASIC PROPERTIES: ~S\n" basic-properties)

		     ;; FIXME: dialog iso window?
		     ;; http://www.gtk.org/tutorial/sec-textentries.html
		     (let ((window (make <gtk-window>))
			   (vbox (make <gtk-vbox>))
			   (ok (make <gtk-button> #:label "Ok")))
		       
		       (add window vbox)
		       (connect ok 'clicked (lambda (b) (destroy window)))

		       (for-each
			(lambda (x)
			  (let ((label (make <gtk-label>
					 ;;#:label (symbol->string (car x))))
					 #:label (format #f "~S" (car x))))
				;;(symbol->string (car x))))
				(entry (make <gtk-entry>
					 #:text (format #f "~S" (cdr x))))
				(hbox (make <gtk-hbox>)))
			    (add hbox label)
			    (add hbox entry)
			    (set-size-request label 150 BUTTON-HEIGHT)
			    (add vbox hbox)))
			(append properties basic-properties))
		       (add vbox ok)
		       
		       (show-all window)
		       (move window x y))))))))
    
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
	  ((and (eq? keyval gdk:s)
		(equal? mods '(control-mask modifier-mask)))
		(save-tweaks go))
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
