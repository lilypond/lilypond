;;;; output-gnome.scm -- implement GNOME canvas output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

;;; HIP -- hack in progress
;;;
;;; You need:
;;;
;;;   * guile-1.6.4 (NOT CVS)
;;;   * Rotty's g-wrap--tng, possibly Janneke's if you have libffi-3.4.
;;;
;;; see also: guile-gtk-general@gnu.org
;;;
;;; Try it
;;;
;;;   * If using GUILE CVS , then compile LilyPond with GUILE 1.6, 
;;;
;;;    PATH=/usr/bin/:$PATH ./configure --enable-config=g16  ; make conf=g16
;;;
;;;   * Install gnome/gtk development stuff and g-wrap, guile-gnome
;;;     see buildscripts/guile-gnome.sh
;;;  
;;;   * Use latin1 encoding for gnome backend, do
;;;
"
       ./configure --prefix=$(pwd) --enable-config=g16
       make -C mf conf=g16 clean
       make -C mf conf=g16 ENCODING_FILE=$(kpsewhich cork.enc)
       (cd mf/out-g16 && mkfontdir)
       xset +fp $(pwd)/mf/out-g16
"
;;;
;;;   * Setup environment
"
export GUILE_LOAD_PATH=$HOME/usr/pkg/g-wrap/share/guile/site:$HOME/usr/pkg/g-wrap/share/guile/site/g-wrap:$HOME/usr/pkg/guile-gnome/share/guile
export LD_LIBRARY_PATH=$HOME/usr/pkg/g-wrap/lib:$HOME/usr/pkg/guile-gnome/lib
export XEDITOR='/usr/bin/emacsclient --no-wait +%l:%c %f'
"
;;;  * For GNOME point-and-click, add
;;;     #(ly:set-point-and-click 'line-column)
;;;    to your .ly; just click an object on the canvas.
;;;
;;;  * Run lily:
"
lilypond-bin -fgnome input/simple-song.ly
"


;;; TODO:
;;;  * pango+feta font (see archives gtk-i18n-list@gnome.org and
;;;    lilypond-devel)
;;;    - wait for/help with pango 1.6
;;;    - convert feta to OpenType (CFF) or TrueType (fontforge?)
;;;    - hack feta20/feta20.pfa?:
;;;  * font, canvas, scaling?
;;;  * implement missing stencil functions
;;;  * implement missing commands
;;;  * user-interface, keybindings
;;;  * cleanups: (too many) global vars
;;;  * papersize, outputscale from book


;;; SCRIPT moved to buildscripts/guile-gnome.sh

(debug-enable 'backtrace)

;;(define-module (scm output-gnome))
(define-module (scm output-gnome)
  #:export (
	    char
	    comment
	    define-origin
	    filledbox
	    horizontal-line
	    no-origin
	    placebox
	    round-filled-box
	    text
	    ))

(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (srfi srfi-13)
 (lily)
 (gnome gtk)
 
 ;; Hmm, <gnome-outputter> is not imported -- but trying this breaks
 ;; framework-gnome in a weird way.
 ;;(scm framework-gnome))
 )

;; the name of the module will change to canvas rsn
(if (resolve-module '(gnome gw canvas))
    (use-modules (gnome gw canvas))
    (use-modules (gnome gw libgnomecanvas)))

;; ughughughughu ughr huh?? -- defined in framework-gnome
(define PIXELS-PER-UNIT 2)
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


(define (dummy . foo) #f)

;; minimal intercept list:
(define output-interface-intercept
  '(comment
    define-origin
    no-origin))

(map (lambda (x) (module-define! this-module x dummy))
     output-interface-intercept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; output-scale and font-size fun
;; This used to be:
(define USED-TO-BE-OUTPUT-SCALE 2.83464566929134)
;; However, it seems that we currently have:
(define 2.3.4-OUTPUT-SCALE 1.75729901757299)
;; to go from ly-units to <MM/points/whatever?>
;; Hmm, is this the source of font size problems wrt titling's right margin?

;;(define pixels-per-unit 1.0)
;;(define ARBITRARY-OUTPUT-SCALE 5)

;; Anyway, for on-screen this does not matter: 2 * 2.5 looks fine
(define pixels-per-unit 2.0)
(define ARBITRARY-OUTPUT-SCALE 2.5)

;;(define output-scale (* OUTPUT-SCALE pixels-per-unit))
(define output-scale (* ARBITRARY-OUTPUT-SCALE pixels-per-unit))



;; helper functions -- sort this out
(define (stderr string . rest)
  ;; debugging
  (if #f
      (begin
	(apply format (cons (current-error-port) (cons string rest)))
	(force-output (current-error-port)))))

(define (utf8 i)
  (cond
   ((< i #x80) (make-string 1 (integer->char i)))
   ((< i #x800) (list->string
		 (map integer->char
		      (list (+ #xc0 (quotient i #x40))
			    (+ #x80 (modulo i #x40))))))
   ((< i #x10000)
    (let ((x (quotient i #x1000))
	  (y (modulo i #x1000)))
      (list->string
       (map integer->char
	    (list (+ #xe0 x)
		  (+ #x80 (quotient y #x40))
		  (+ #x80 (modulo y #x40)))))))
   (else FIXME)))
  
(define (custom-utf8 i)
  (if (< i 80)
      (utf8 i)
      (utf8 (+ #xee00 i))))

(define (draw-rectangle x1 y1 x2 y2 color width-units)
  (make <gnome-canvas-rect>
    #:parent (root (canvas go)) #:x1 x1 #:y1 y1 #:x2 x2 #:y2 y2
    #:fill-color color #:width-units width-units))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; stencil outputters
;;;;

(define (char font i)
  (text font (utf8 i)))

(define (placebox x y expr)
  (stderr "item: ~S\n" expr)
  (let ((item expr))
    ;;(if item
    ;; FIXME ugly hack to skip #unspecified ...
    (if (and item (not (eq? item (if #f #f))))
	(begin
	  (move item
		(* output-scale (+ (car system-origin) x))
		(* output-scale (- (car system-origin) y)))
	  (affine-relative item output-scale 0 0 output-scale 0 0)
	  
	  (gtype-instance-signal-connect item 'event item-event)
	  (if (location go)
	      (hashq-set! (item-locations go) item (location go)))
	  item)
	#f)))

(define (round-filled-box breapth width depth height blot-diameter)
  ;; FIXME: no rounded corners on rectangle...
  ;; FIXME: blot?
  (draw-rectangle (- breapth) depth width (- height) "black" blot-diameter))

(define (pango-font-name font)
  (cond
   ((equal? (ly:font-name font) "GNU-LilyPond-feta-20")
    "lilypond-feta, regular 32")
   (else
    "ecrm12")))
    ;;(ly:font-name font))))
    ;;(ly:font-filename font))))

(define (pango-font-size font)
  (let* ((designsize (ly:font-design-size font))
	 (magnification (* (ly:font-magnification font)))
	 
	 ;; experimental sizing:
	 ;; where does factor come from?
	 ;;
	 ;; 0.435 * (12 / 20) = 0.261
	 ;; 2.8346456692913/ 0.261 = 10.86071137659501915708
	 ;;(ops (* 0.435 (/ 12 20) (* output-scale pixels-per-unit)))
	 ;; for size-points
	 (ops 2.61)
	 
	 (scaling (* ops magnification designsize)))
    (stderr "OPS:~S\n" ops)
    (stderr "scaling:~S\n" scaling)
    (stderr "magnification:~S\n" magnification)
    (stderr "design:~S\n" designsize)
    
    scaling))

;;font-name: "GNU-LilyPond-feta-20"
;;font-filename: "feta20"
;;pango-font-name: "lilypond-feta, regular 32"
;;OPS:2.61
;;scaling:29.7046771653543
;;magnification:0.569055118110236
;;design:20.0

(define (text font string)
  (stderr "font-name: ~S\n" (ly:font-name font))
  ;; TODO s/filename/file-name/
  (stderr "font-filename: ~S\n" (ly:font-filename font))
  
  (stderr "pango-font-name: ~S\n" (pango-font-name font))
  (stderr "pango-font-size: ~S\n" (pango-font-size font))
  (let ((item
	 (make <gnome-canvas-text>
	   #:parent (root (canvas go))
      
	   ;; experimental text placement corrections.
	   ;; UGHR?  What happened to tex offsets?  south-west?
	   ;; is pango doing something 'smart' wrt baseline ?
	   #:anchor 'south-west
	   #:x 0.003 #:y 0.123
	   
	   ;;
	   ;;#:anchor 'west
	   ;;#:x 0.015 #:y -3.71
	   
	   #:font (pango-font-name font)
	   
	   #:size-points (pango-font-size font)
	   ;;#:size ...
	   #:size-set #t
	   
	   ;;apparently no effect :-(
	   ;;#:scale 1.0
	   ;;#:scale-set #t
	   
	   #:fill-color "black"
	   #:text string)))
    (set! (text-items go) (cons item (text-items go)))
    item))

(define (filledbox a b c d)
  (round-filled-box a b c d 0.001))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 thickness)
  ;;(let ((thickness 2))
  (filledbox (- x1) (- x2 x1) (* .5 thickness) (* .5 thickness)))

;; origin -- bad name
(define (define-origin file line col)
  ;; ughr, why is this not passed as [part of] stencil object
  (set! (location go) (if (procedure? point-and-click)
			  ;; duh, only silly string append
			  ;; (point-and-click line col file)
			  (list line col file)
			  #f)))
