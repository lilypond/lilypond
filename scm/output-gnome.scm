;;;; output-gnome.scm -- implement GNOME canvas output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>


;;; HIP -- hack in progress

"
## install gnome-devel

## use guile-1.6 for g-wrap/guile-gnome
PATH=/usr/bin:$PATH

## get g-wrap 2.0
tla register-archive http://people.debian.org/~rotty/arch/guile-gnome-devel@gnu.org/2004/4 || true

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
rm -rf gg-pristine
tla get a.rottmann@gmx.at--2004-main/guile-gnome-dists--dev gg-pristine
cd gg-pristine
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
#fixup
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
 (gnome gtk))


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
	    ""))))))
  

;;; Global vars
(define main-window #f)
(define the-canvas #f)

(define output-scale (* 2 2.83464566929134))
(define system-y 0)
(define line-thickness 0.001)


(define (char font i)
  #f)

(define (placebox x y expr)
  #f)

;; gnome_canvas_item_new (gnome_canvas_root (canvas),
;;  gnome_canvas_rect_get_type (),
;;  "x1", (double) x1,
;;  "y1", (double) y1,
;;  "x2", (double) x2,
;;  "y2", (double) y2,
;;  "fill_color", "black",
;;  "outline_color", "black",
;;  "width_units", 1.0,
;;  NULL);
  
(define (round-filled-box breapth width depth height blot-diameter)
  (let* ((x . ,(number->string (* output-scale (- 0 breapth))))
	 (y . ,(number->string (* output-scale (- 0 height))))
	 (width . ,(number->string (* output-scale (+ breapth width))))
	 (height . ,(number->string (* output-scale (+ depth height))))
	 (ry . ,(number->string (/ blot-diameter 2)))
	 ;;(item (make <canvas-item>
	 ;;	 #:type 'GnomeCanvasLine
	 ;;	 #:points '(x y width height))
	 )
  #f))

(define (fontify font expr)
  #f)

(define (end-output)
  (gtk-main))

(define (header . rest)
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 ;;(canvas (make <canvas>))
	 ;;(canvas (make <gnome-canvas>))
	 (button (make <gtk-button> #:label "Hello, World!")))

    (gtk-container-set-border-width window 10)
    (gtk-container-add window button)
    
    (gtype-instance-signal-connect button 'clicked
				   (lambda (b) (gtk-main-quit)))

    (gtk-widget-show-all window)
    (set! main-window window)
    ;;(set! the-canvas canvas))
    ))

(define (text . rest)
  #f)

(define (filledbox a b c d)
  (round-filled-box a b c d 0.001))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 th)
  (filledbox (- x1) (- x2 x1) (* .5 th) (* .5 th)))
