;;;; output-gnome.scm -- implement GNOME canvas output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>


;;; HIP -- hack in progress
;;;
;;; status: hello-world
;;;
;;; This first working version needs rotty's g-wrap--tng
;;; and janneke's guile-gnome patches, instructions below.
;;;
;;; Try it:
;;;     lilypond-bin -fgnome input/simple-song.ly
;;;

;;; TODO: pango+feta font (wait for pango 1.6?)

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
## ugh: get janneke's stuff -- should make build-config, I guess?
tla register-archive janneke@gnu.org--2004-gnome http://lilypond.org/~janneke/{arch}/2004-gnome || true
rm -rf defs
rm -rf gtk
rm -rf libgnomecanvas
tla get janneke@gnu.org--2004-gnome/libgnomecanvas--janneke--0 libgnomecanvas
tla get janneke@gnu.org--2004-gnome/libgnomecanvas--janneke--0 defs
tla get janneke@gnu.org--2004-gnome/libgnomecanvas--janneke--0 gtk
tla get janneke@gnu.org--2004-gnome/libgnomecanvas--janneke--0 libgnomecanvas

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


lilypond-bin -fgnome input/simple-song.ly


"



(debug-enable 'backtrace)

(define-module (scm output-gnome))
(define this-module (current-module))

(use-modules
 (guile)
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

;; helper functions
(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

(define (item-event item event . data)
  (case (gdk-event:type event)
    ((enter-notify) (gobject-set-property item 'fill-color "white"))
    ((leave-notify) (gobject-set-property item 'fill-color "black"))
    ((2button-press) (gobject-set-property item 'fill-color "red")))
  #t)
    
;;; Global vars
(define main-window #f)
(define canvas-root #f)

(define system-origin '(0 . 0))

(define canvas-width 400)
(define canvas-height
  (inexact->exact (round (* 1.42 canvas-width))))

(define output-scale (* 2 2.83464566929134))
;;(define output-scale 2.83464566929134)
;;(define output-scale 1)

(define (char font i)
  ;;(text font (make-string 1 (integer->char i))))
  (text font "a"))

(define (placebox x y expr)
  (let ((item expr))
    (if item
	(begin
	  (move item
		(* output-scale (+ (car system-origin) x))
		(* output-scale (- (car system-origin) y)))
	  (affine-relative item output-scale 0 0 output-scale 0 0)
	  
	  (gtype-instance-signal-connect item 'event item-event)
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
    
    (show-all window)
    (set! canvas-root (root canvas))
    (set! main-window window)))

(define (text font string)
  (make <gnome-canvas-text>
    #:parent canvas-root
    #:x 0 #:y 0
    #:size-points 12
    #:size-set #t
    #:font "new century schoolbook, i bold 20"
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

