;;;; output-gnome.scm -- implement GNOME canvas output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Jan Nieuwenhuizen <janneke@gnu.org>

;;; TODO:
;;;
;;;  * Figure out and fix font scaling and character placement
;;;  * EC font package: add missing X font directories and AFMs
;;;  * User-interface, keybindings
;;;  * Implement missing stencil functions
;;;  * Implement missing commands
;;;  * More information in stencils, e.g., location and grob tag.
;;;  * Embedded Lily:
;;;    - allow GnomeCanvas or `toplevel' GtkWindow to be created
;;;      outside of LilyPond
;;;    - lilylib.
;;;  * Release schedule and packaging of dependencies.  This hack
;;;    depends on several CVS and TLA development sources.

;;; You need:
;;;
;;;   * Rotty's g-wrap >= 1.9.1 (or TLA)
;;;   * guile-gnome-platform >= 2.5.992 (or TLA)
;;;   * pango >= 1.5.2 (or CVS)
;;;
;;; See also: guile-gtk-general@gnu.org

;;; Try it
;;;
;;;   [* Get cvs and tla]
;;;
;;;   * Install gnome/gtk and libffi development stuff
;;;
;;;   * Install pango, g-wrap and guile-gnome from CVS or arch: 
;;;     see buildscripts/guile-gnome.sh
;;;  
;;;   * Build LilyPond with gui support: configure --enable-gui
;;;
;;;   * Supposing that LilyPond was built in ~/cvs/savannah/lilypond,
;;;     tell fontconfig about the feta fonts dir:
"
cat > ~/.fonts.conf << EOF
<fontconfig>
<dir>~/cvs/savannah/lilypond/mf/out</dir>
</fontconfig>
EOF
"
;;;     or copy all your .pfa/.pfb's to ~/.fonts if your fontconfig
;;;     already looks there for fonts.  Check if it works by doing:
"
fc-list | grep -i lily
"
;;;
;;;   * Setup environment
"
export GUILE_LOAD_PATH=$HOME/usr/pkg/g-wrap/share/guile/site:$HOME/usr/pkg/g-wrap/share/guile/site/g-wrap:$HOME/usr/pkg/guile-gnome/share/guile:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=$HOME/usr/pkg/pango/lib:$HOME/usr/pkg/g-wrap/lib:$HOME/usr/pkg/guile-gnome/lib:$LD_LIBRARY_PATH
export XEDITOR='/usr/bin/emacsclient --no-wait +%l:%c %f'
"
;;;  * Also for GNOME point-and-click, you need to set XEDITOR and add
"
#(ly:set-point-and-click 'line-column)
"
;;;    to your .ly.
;;;
;;;  * Run lily:
"
lilypond -fgnome input/simple-song.ly
"
;;; point-and-click: (mouse-1) click on a graphical object;
;;; grob-property-list: (mouse-3) click on a graphical object.

(debug-enable 'backtrace)

(define-module (scm output-gnome))
(define this-module (current-module))

(use-modules
 (guile)
 (srfi srfi-13)
 (lily)
 (gnome gtk))


;; The name of the module will change to `canvas' rsn
(if (resolve-module '(gnome gw canvas))
    (use-modules (gnome gw canvas))
    (use-modules (gnome gw libgnomecanvas)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; globals

;; junkme
(define system-origin '(0 . 0))

;;; set by framework-gnome.scm
(define canvas-root #f)
(define output-scale #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

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
    #:parent (canvas-root) #:x1 x1 #:y1 y1 #:x2 x2 #:y2 y2
    #:fill-color color #:width-units width-units))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stencil outputters
;;;

;;; catch-all for missing stuff
;;; comment this out to see find out what functions you miss :-)
(define (dummy . foo) #f)
(map (lambda (x) (module-define! this-module x dummy))
     (append
      (ly:all-stencil-expressions)
      (ly:all-output-backend-commands)))



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
    ;; FIXME
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
  
  (make <gnome-canvas-text>
    #:parent (canvas-root)

    #:anchor 'west
    #:x 0.0 #:y 0.0
    
    #:font (pango-font-name font)
    
    #:size-points (pango-font-size font)
    ;;#:size ...
    #:size-set #t
    
    ;;apparently no effect :-(
    ;;#:scale 1.0
    ;;#:scale-set #t
    
    #:fill-color "black"
    #:text string))

(define (filledbox a b c d)
  (round-filled-box a b c d 0.001))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 thickness)
  (filledbox (- x1) (- x2 x1) (* .5 thickness) (* .5 thickness)))

;;(define (define-origin file line col)
;;  (if (procedure? point-and-click)
;;      (list 'location line col file)))

(define (grob-cause grob)
  grob)
