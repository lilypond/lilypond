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
;;;   * lilypond branch: lilypond_2_3_2b; the framework-* backend
;;;     loads output-gnome.scm at startup, which seems to break g-wrapped
;;;     goops.
;;;
;;; see also: guile-gtk-general@gnu.org
;;;
;;; Try it
;;;
;;;   * Install g-wrap, guile-gnome (see script below)
;;;  
;;;   * Use latin1 encoding for gnome backend, do
;;;       make -C mf clean
;;;       make -C mf ENCODING_FILE=$(kpsewhich cork.enc)
;;;       (cd mf/out && mkfontdir)
;;;       xset +fp $(pwd)/mf/out
;;;
;;;   * Setup PATHs:

"
# do not use guile CVS:
export PATH=/usr/bin/:$PATH
# use g-wrap and guile-gnome from usr/pkg
export GUILE_LOAD_PATH=$HOME/usr/pkg/g-wrap/share/guile/site:$HOME/usr/pkg/g-wrap/share/guile/site/g-wrap:$HOME/usr/pkg/guile-gnome/share/guile
export LD_LIBRARY_PATH=$HOME/usr/pkg/g-wrap/lib:$HOME/usr/pkg/guile-gnome/lib
"

;;;  * Set XEDITOR and add
;;;     #(ly:set-point-and-click 'line-column)
;;;    to your .ly to get point-and-click
;;;
;;;  * Run lily: lilypond-bin -fgnome input/simple-song.ly
;;;
;;;
;;;      todo: hmm --output-base broken?
;;;   ### cd mf && mftrace --encoding=$(kpsewhich cork.enc) --autotrace --output-base=feta-cork-20 feta20.mf && mv feta20.pfa out

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

;;; Note: this install information is volatile
;;;       you'll probably want to pull all from
;;;       from guile-gnome-devel@gnu.org--2004 soon
;;;   

"
#!/bin/bash

set -ex

if  [ -d $HOME/usr/pkg/libffi/ ] ; then
 export LDFLAGS=-L$HOME/usr/pkg/libffi/lib/
 export CPPFLAGS=-I$HOME/usr/pkg/libffi/include
fi 

export AUTOMAKE=automake-1.8
export AUTOCONF=autoconf2.50 

rm -rf test
mkdir test
cd test

## 1.  install gnome-devel (Debian/unstable: apt-get install gnome-devel)

## 2.  *** NOTE: use guile-1.6 for g-wrap and guile-gnome ***
##### using GUILE CVS g-wrap/guile-gnome is experimental (read: segfaults)
PATH=/usr/bin:$PATH


## 3.  get g-wrap 2.0
tla register-archive a.rottmann@gmx.at--2004-main http://people.debian.org/~rotty/arch/a.rottmann@gmx.at/2004-main || true

rm -rf g-wrap
## tla get a.rottmann@gmx.at--2004-main/g-wrap--tng g-wrap
## pull latest g-wrap from janneke -- this step is probably no longer
## necessary when you read this
tla register-archive janneke@gnu.org--2004-gnome http://lilypond.org/~janneke/{arch}/2004-gnome || true
tla get janneke@gnu.org--2004-gnome/g-wrap--janneke g-wrap
cd g-wrap

rm -rf $HOME/usr/pkg/g-wrap
sh autogen.sh --noconfigure
mkdir =build
cd =build
../configure --prefix=$HOME/usr/pkg/g-wrap
make install

# cp srfi-34.scm from CVS head ?  --hwn
(cd $HOME/usr/pkg/g-wrap/share/guile/site
 mv srfi-34.scm srfi-34.scm-g-wrap
 cp $HOME/usr/pkg/guile/share/guile-1.7/srfi/srfi-34.scm .)

cd ../..

## 4.  get guile-gnome
tla register-archive guile-gnome-devel@gnu.org--2004 http://people.debian.org/~rotty/arch/guile-gnome-devel@gnu.org/2004/ || true
rm -rf guile-gnome
tla get guile-gnome-devel@gnu.org--2004/dists--dev guile-gnome
cd guile-gnome
tla build-config -r configs/gnu.org/dev
cd src

## 5.  get the gnome canvas module
tla get guile-gnome-devel@gnu.org--2004/libgnomecanvas--dev libgnomecanvas

## pull latest defs from janneke -- this step is probably no longer
## necessary when you read this
## tla register-archive janneke@gnu.org--2004-gnome http://lilypond.org/~janneke/{arch}/2004-gnome || true
## rm -rf defs
## tla get janneke@gnu.org--2004-gnome/defs--janneke defs

rm -rf $HOME/usr/pkg/guile-gnome
sh autogen.sh --noconfigure
mkdir ../=build
cd ../=build

export GUILE_LOAD_PATH=$HOME/usr/pkg/g-wrap/share/guile/site:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$HOME/usr/pkg/g-wrap/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/usr/pkg/g-wrap/lib/pkgconfig:$PKG_CONFIG_PATH

../src/configure --prefix=$HOME/usr/pkg/guile-gnome


# requires 800mb RAM with -O2
(cd libgnomecanvas/gnome/gw; perl  -i~  -pe 's/-O2//g' Makefile)
    
G_WRAP_MODULE_DIR=$HOME/usr/pkg/g-wrap/share/guile/site make install

export GUILE_LOAD_PATH=$HOME/usr/pkg/guile-gnome/share/guile:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$HOME/usr/pkg/guile-gnome/lib:$LD_LIBRARY_PATH
guile -s ../src/libgnomecanvas/examples/canvas.scm


# simple test
guile -s ../src/libgnomecanvas/examples/canvas.scm

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
 ;; the name of the module will change to canvas rsn
 ;;(gnome gw libgnomecanvas))
 (gnome gw canvas))


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
  '(comment
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
    stop-system))

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
(define main-scrolled #f)
(define main-canvas #f)
(define canvas-root #f)
(define page-number 0)

(define page-stencils #f)
(define output-canvas #f)

(define system-origin '(0 . 0))

;; UGHr
(define item-locations (make-hash-table 31))
(define location #f)

(define canvas-width 400)
(define canvas-height
  (inexact->exact (round (* 1.42 canvas-width))))

(define font-paper #f)

;;(define pixels-per-unit 1.0)
(define pixels-per-unit 2.0)

;; TODO: use canvas scaling, use output-scale for paper/canvas dimensions?
;;(define output-scale (* 2 2.83464566929134))
;;(define output-scale 2.83464566929134)
(define OUTPUT-SCALE 2.83464566929134)
(define output-scale (* OUTPUT-SCALE pixels-per-unit))
;;(define output-scale 1)

;; helper functions
(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

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

;; TODO: one list per-page
(define text-items '())
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
	  (if location
	      (hashq-set! item-locations item location))
	  item)
	#f)))

(define (round-filled-box breapth width depth height blot-diameter)
  ;; FIXME: no rounded corners on rectangle
  (make <gnome-canvas-rect>
    #:parent canvas-root
    #:x1 (- breapth) #:y1 depth #:x2 width #:y2 (- height)
    #:fill-color "black" #:width-units blot-diameter))

;;(define (fontify font expr)
;;  #f)

(define (main outputter pages)
  (let* ((window (make <gtk-window> #:type 'toplevel))
	 (button (make <gtk-button> #:label "Exit"))
	 (next (make <gtk-button> #:label "Next"))
	 (prev (make <gtk-button> #:label "Previous"))
	 (canvas (make <gnome-canvas>))
	 (vbox (make <gtk-vbox> #:homogeneous #f))
	 (hbox (make <gtk-hbox> #:homogeneous #f))
	 (scrolled (make <gtk-scrolled-window>)))

    (add window vbox)
    (add vbox scrolled)
    (add scrolled canvas)

    ;;(set-size-request button canvas-width 20)
    ;;(add vbox button)
    ;;(set-child-packing vbox button #f #f 0 'end)
    
    (add vbox hbox)
    (set-size-request hbox canvas-width 25)
    (set-child-packing vbox hbox #f #f 0 'end)
    
    (set-child-packing hbox button #f #f 0 'end)
    ;;(set-size-request next 40 25)
    ;;(set-size-request prev 40 25)
    (set-size-request button (/ canvas-width 2) 25)
    
    (add hbox next)
    (add hbox prev)
    (add hbox button)
    
    
    (gtype-instance-signal-connect button 'clicked
				   (lambda (b) (gtk-main-quit)))
    (gtype-instance-signal-connect next 'clicked
				   (lambda (b) (dump-page (1+ page-number))))
    (gtype-instance-signal-connect prev 'clicked
				   (lambda (b) (dump-page (1- page-number))))
    
    ;; papersize
    (set-size-request canvas canvas-width canvas-height)
    (set-scroll-region canvas 0 0 2000 4000)
    (set-pixels-per-unit canvas pixels-per-unit)
    
    (gtype-instance-signal-connect window 'key-press-event key-press-event)
    (show-all window)

    ;; HMMM
    (set! canvas-root (root canvas))
    (set! main-canvas canvas)
    (set! main-window window)
    (set! output-canvas outputter)
    (set! page-stencils pages)
    (set! main-scrolled scrolled)
    
    (dump-page 0)
    (gtk-main)))

(define (pango-font-name font)
  (cond
   ((equal? (ly:font-name font) "GNU-LilyPond-feta-20")
    "lilypond-feta, regular 32")
   (else
    (ly:font-filename font))))

(define (pango-font-size font)
  (let* ((designsize (ly:font-design-size font))
	 (magnification (* (ly:font-magnification font)))
	 ;;(ops (ly:paper-lookup paper 'outputscale))
	 ;;(ops (* pixels-per-unit OUTPUT-SCALE))
	 ;;(ops (* pixels-per-unit pixels-per-unit))
	 (ops (* (/ 12 20) (* pixels-per-unit pixels-per-unit)))
	 (scaling (* ops magnification designsize)))
    scaling))

(define (text font string)
  (stderr "font-name: ~S\n" (ly:font-name font))
  ;; TODO s/filename/file-name/
  (stderr "font-filename: ~S\n" (ly:font-filename font))
  
  (stderr "pango-font-name: ~S\n" (pango-font-name font))
  (stderr "pango-font-size: ~S\n" (pango-font-size font))
  (set!
   text-items
   (cons
    (make <gnome-canvas-text>
      #:parent canvas-root
      #:x 0 #:y 0
      ;;    #:font "new century schoolbook, i bold 20"
      #:font (pango-font-name font)
      ;; #:size-points 12
      #:size-points (pango-font-size font)
      ;;#:size (pango-font-size font)
      #:size-set #t

      ;;apparently no effect :-(
      ;;#:scale 1.0
      ;;#:scale-set #t
      
      #:fill-color "black"
      #:text string
      #:anchor 'west)
    text-items))
  (car text-items))

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

(define (dump-page number)
  (if (or (not page-stencils)
	  (< number 0)
	  (>= number (vector-length page-stencils)))
      (stderr "No such page: ~S\n" (1+ number))
      (begin
	(set! page-number number)

	;; no destroy method for gnome-canvas-text?
	;;(map destroy (gtk-container-get-children main-canvas))
	;;(map destroy text-items)

	;; UGHR - destroying the whole canvas....
	(if (and main-canvas
		 (not (null? text-items)))
	    (let* ((canvas (make <gnome-canvas>))
		   (root (root canvas)))
	      
	      (destroy main-canvas)
	      (add main-scrolled canvas)

	      ;; papersize
	      (set-size-request canvas canvas-width canvas-height)
	      (set-scroll-region canvas 0 0 2000 4000)
	      (set-pixels-per-unit canvas pixels-per-unit)
	      (show canvas)
	      
	      (set! main-canvas canvas)
	      (set! canvas-root root)
	      (set! text-items '())))
	
	(ly:outputter-dump-stencil output-canvas
				   (vector-ref page-stencils page-number)))))

