;;;; output-gnome.scm -- implement GNOME canvas output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>


;;; HIP -- hack in progress
;;;
;;; status: hello-world
;;;
;;; This first working version needs rotty's g-wrap--tng.
;;; (janneke's guile-gnome patches now in main archive).
;;;
;;; Try it:
;;;     lilypond-bin -fgnome input/simple-song.ly

;;; Set XEDITOR and add
;;;    #(ly:set-point-and-click 'line-column)
;;; to your .ly to get point-and-click

;;; TODO:
;;;  * pango+feta font (see archives gtk-i18n-list@gnome.org and
;;;    lilypond-devel)
;;;    - wait for/help with pango 1.6
;;;    - convert feta to OpenType (CFF) or TrueType (fontforge?)
;;;    - hack feta20/feta20.pfa?: use latin1 encoding for gnome backend
;;;      Trying:
;;;         mftrace --encoding=$(kpsewhich cork.enc) --autotrace --output-base=feta-cork-20 feta20.mf
;;;      hmm --output-base broken?
;;;  * implement missing stencil functions
;;;  * implement missing commands (next, prev? page)
;;;  * user-interface, keybindings

;;; Note: this install information is volatile
;;;       you'll probably want to pull all from
;;;       from guile-gnome-devel@gnu.org--2004 soon
;;;   
;;; move this into workbook?

"
## install gnome-devel

## use guile-1.6 for g-wrap/guile-gnome
PATH=/usr/bin:$PATH

## get g-wrap 2.0
tla register-archive a.rottmann@gmx.at--2004-main http://people.debian.org/~rotty/arch/a.rottmann@gmx.at/2004-main || true

rm -rf gw-pristine
tla get a.rottmann@gmx.at--2004-main/g-wrap--tng gw-pristine
cd gw-pristine

AUTOMAKE=automake-1.8 AUTOCONF=autoconf2.50 sh autogen.sh --noconfigure
mkdir =build
cd =build
../configure --prefix=$HOME/usr/pkg/g-wrap
make install

cd ../..

## get guile-gnome
tla register-archive guile-gnome-devel@gnu.org--2004 http://people.debian.org/~rotty/arch/guile-gnome-devel@gnu.org/2004/ || true
rm -rf guile-gnome
tla guile-gnome-devel@gnu.org--2004/dists--dev guile-gnome
cd guile-gnome
tla build-config -r configs/gnu.org/dev
cd src

AUTOMAKE=automake-1.8 AUTOCONF=autoconf2.50 sh autogen.sh --noconfigure
mkdir ../=build
cd ../=build

export GUILE_LOAD_PATH=$HOME/usr/pkg/g-wrap/share/guile/site:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$HOME/usr/pkg/g-wrap/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/usr/pkg/g-wrap/lib/pkgconfig:$PKG_CONFIG_PATH

../src/configure --prefix=$HOME/usr/pkg/guile-gnome

G_WRAP_MODULE_DIR=$HOME/usr/pkg/g-wrap/share/guile/site make install
#FIXME: fixup
(cd $HOME/usr/pkg/guile-gnome/share/guile/gnome && mv gtk/g[dt]k.scm gw)

export GUILE_LOAD_PATH=$HOME/usr/pkg/guile-gnome/share/guile:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$HOME/usr/pkg/guile-gnome/lib:$LD_LIBRARY_PATH
guile -s ../src/gtk/examples/hello.scm


"



(debug-enable 'backtrace)

(define-module (scm output-gnome))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (srfi srfi-13)
 (lily)
 (gnome gtk)
 (gnome gtk gdk-event)
 (gnome gw libgnomecanvas))


;;; Lily output interface --- fix silly names and docme

"
 The output interface has functions for
  * formatting stencils, and
  * output commands

 Stencils:
 beam
 bezier-sandwich
 bracket
 ...

 Commands:
 define-fonts
 header
 placebox
 ...


 The Bare minimum interface for \score { \notes c } } should
 implement:

    INTERFACE-output-expression
    char
    filledbox
    placebox

 and should intercept:
"

(define (dummy . foo) #f)

;; minimal intercept list:
(define output-interface-intercept
  '(
    comment
    define-fonts
    end-output
    header
    header-end
    lily-def
    no-origin
    output-scopes
    start-page
    stop-page
    start-system
    stop-system
 ))

(map (lambda (x) (module-define! this-module x dummy))
     output-interface-intercept)

(define-public (gnome-output-expression expr port)
  (display (dispatch expr) port))

(define (dispatch expr)
  (if (pair? expr)
      (let ((keyword (car expr)))
	(cond
	 ((eq? keyword 'some-func) "")
	 ;;((eq? keyword 'placebox) (dispatch (cadddr expr)))
	 (else
	  (if (module-defined? this-module keyword)
	      (apply (eval keyword this-module) (cdr expr))
	      (begin
		(display
		 (string-append "undefined: " (symbol->string keyword) "\n"))
		"")))))
      expr))

;;; Global vars
(define main-window #f)
(define main-canvas #f)
(define canvas-root #f)

(define system-origin '(0 . 0))

;; UGHr
(define item-locations (make-hash-table 31))
(define location #f)

(define canvas-width 400)
(define canvas-height
  (inexact->exact (round (* 1.42 canvas-width))))

;; TODO: use canvas scaling, use output-scale for paper/canvas dimensions?
(define output-scale (* 2 2.83464566929134))
;;(define output-scale 2.83464566929134)
;;(define output-scale 1)

;; helper functions
(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))


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
    ((enter-notify) (gobject-set-property item 'fill-color "white"))
    ((leave-notify) (gobject-set-property item 'fill-color "black"))
    ((button-press)
     (let ((location (hashq-ref item-locations item #f)))
       (if location
	   (location-callback location)
	   (stderr "no location\n"))))
    ((2button-press) (gobject-set-property item 'fill-color "red")))
  #t)

(define pixels-per-unit 1.0)
(define (key-press-event item event . data)
  (let ((keyval (gdk-event-key:keyval event))
	(mods (gdk-event-key:modifiers event)))
    (cond ((and (or (eq? keyval gdk:q)
		    (eq? keyval gdk:w))
		(equal? mods '(control-mask modifier-mask)))
	   (gtk-main-quit))
	  ((and #t ;;(null? mods)
		(eq? keyval gdk:plus))
	   (set! pixels-per-unit (* pixels-per-unit 2))
	   (set-pixels-per-unit main-canvas pixels-per-unit))
	  ((and #t ;; (null? mods)
		(eq? keyval gdk:minus))
	   (set! pixels-per-unit (/ pixels-per-unit 2))
	   (set-pixels-per-unit main-canvas pixels-per-unit)))
    #f))

(define (char font i)
  ;;(text font (make-string 1 (integer->char i))))
  ;;(text font "a"))
  ;; FIXME: utf8?
  (if (< i 127)
      (text font (make-string 1 (integer->char i)))
      (text font "a")))

(define (placebox x y expr)
  (let ((item expr))
    (if item
	(begin
	  (move item
		(* output-scale (+ (car system-origin) x))
		(* output-scale (- (car system-origin) y)))
	  (affine-relative item output-scale 0 0 output-scale 0 0)
	  
	  (gtype-instance-signal-connect item 'event item-event)
	  (if location
	      (hashq-set! item-locations item location))
	  item)
	#f)))

(define (round-filled-box breapth width depth height blot-diameter)
  ;; FIXME: no rounded corners on rectangle
  (make <gnome-canvas-rect>
    #:parent canvas-root
    #:x1 (- breapth) #:y1 (- depth) #:x2 width #:y2 height
    #:fill-color "black" #:width-units blot-diameter))

(define (fontify font expr)
  #f)

(define (end-output)
  (gtk-main))

(define (header . rest)
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (button (make <gtk-button> #:label "Exit"))
	 (canvas (make <gnome-canvas>))
	 (vbox (make <gtk-vbox> #:homogeneous #f))
	 (scrolled (make <gtk-scrolled-window>)))

    (add window vbox)
    (add vbox scrolled)
    (add scrolled canvas)

    (set-size-request button canvas-width 20)
    (add vbox button)
    (set-child-packing vbox button #f #f 0 'end)

    (gtype-instance-signal-connect button 'clicked
				   (lambda (b) (gtk-main-quit)))
    
    ;; papersize
    (set-size-request canvas canvas-width canvas-height)
    (set-scroll-region canvas 0 0 2000 4000)
    
    (gtype-instance-signal-connect window 'key-press-event key-press-event)
    
    (show-all window)
    (set! canvas-root (root canvas))
    (set! main-canvas canvas)
    (set! main-window window)))

(define (pango-font-name font)
  (cond
   ((equal? (ly:font-name font) "GNU-LilyPond-feta-20")
    "lilypond-feta, regular 32")
   (else
    (ly:font-filename font))))

(define (text font string)
  (stderr "font-name: ~S\n" (ly:font-name font))
  ;; TODO s/filename/file-name/
  (stderr "font-filename: ~S\n" (ly:font-filename font))
  (make <gnome-canvas-text>
    #:parent canvas-root
    #:x 0 #:y 0
    #:size-points 12
    #:size-set #t
    ;;    #:font "new century schoolbook, i bold 20"
    #:font (pango-font-name font)
    #:fill-color "black"
    #:text string))

(define (filledbox a b c d)
  (round-filled-box a b c d 0.001))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 thickness)
  ;;(let ((thickness 2))
  (filledbox (- x1) (- x2 x1) (* .5 thickness) (* .5 thickness)))

(define (start-system origin . rest)
  (set! system-origin origin))

;; origin -- bad name
(define (define-origin file line col)
  ;; ughr, why is this not passed as [part of] stencil object
  (set! location (if (procedure? point-and-click)
		     ;; duh, only silly string append
		     ;; (point-and-click line col file)
		     (list line col file)
		     #f)))

