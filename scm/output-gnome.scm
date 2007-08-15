;;;; output-gnome.scm -- implement GNOME canvas output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2007 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; TODO:
;;;;
;;;;  * .cff MUST NOT be in fc's fontpath.
;;;;    - workaround: remove mf/out from ~/.fonts.conf,
;;;;      instead add ~/.fonts and symlink all /mf/out/*otf there.
;;;;    - bug in fontconfig/freetype/pango?

;;;  * check: blot+scaling
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
;;;  * Release schedule and packaging of dependencies.
;;;    - g-wrap-1.9.3 is already in incoming.
;;;    - guile-gnome-platform-2.8.0 will probably be packaged early 2005.

;;; You need:
;;;
;;;   * Rotty's g-wrap >= 1.9.3
;;;   * guile-gnome-platform >= 2.7.97
;;;   * pango >= 1.6.0
;;;
;;; See also: guile-gtk-general@gnu.org

;;; Try it
;;;
;;;   * Install gnome/gtk and libffi development stuff
;;;
;;;   * Install [pango, g-wrap and] guile-gnome from source,
;;;     see buildscripts/guile-gnome.sh
;;;  
;;;   * Build LilyPond with gui support: configure --enable-gui
;;;
;;;   * Supposing that LilyPond was built in ~/cvs/savannah/lilypond,
;;;     tell fontconfig about the feta fonts dir and run fc-cache
"
cat > ~/.fonts.conf << EOF
<fontconfig>
<dir>~/cvs/savannah/lilypond/mf/out</dir>
<dir>/usr/share/texmf/fonts/type1/public/ec-fonts-mftraced</dir>
</fontconfig>
EOF
fc-cache
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

(define-module (scm output-gnome))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (srfi srfi-13)
 (lily)
 (gnome gtk)
 (gnome gw canvas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; globals

;;; set by framework-gnome.scm
(define canvas-root #f)
(define output-scale #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(define (utf-8 i)
  (cond
   ((< i #x80) (list (integer->char i)))
   ((< i #x800) (map integer->char
		     (list (+ #xc0 (quotient i #x40))
			   (+ #x80 (modulo i #x40)))))
   ((< i #x10000)
    (let ((x (quotient i #x1000))
	  (y (modulo i #x1000)))
      (map integer->char
	   (list (+ #xe0 x)
		 (+ #x80 (quotient y #x40))
		 (+ #x80 (modulo y #x40))))))
   (else (begin (stderr "programming-error: utf-8 too big:~x\n" i)
		(list (integer->char 32))))))

(define (integer->utf-8-string integer)
  (list->string (utf-8 integer)))

(define (char->utf-8-string char)
  (list->string (utf-8 (char->integer char))))

(define (string->utf-8-string string)
  (apply
   string-append
   (map (lambda (x) (char->utf-8-string x)) (string->list string))))

(define (music-font? font)
  (let ((family (car (font-name-style font))))
    (string=? (substring family 0 (min (string-length family) 10))
	      "Emmentaler")))

;;; FONT may be font smob, or pango font string
(define (pango-font-name font)
  (if (string? font)
      (list font "Regular")
      (apply format (append '(#f "~a, ~a") (font-name-style font)))))

;;; FONT may be font smob, or pango font string
(define (canvas-font-size font)
  ;; FIXME: 1.85?
  (* 1.85
     (if (string? font)
	 12
	 (* output-scale (modified-font-metric-font-scaling font)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrappers from guile-gnome TLA
;;; guile-gnome-devel@gnu.org--2004
;;; http://arch.gna.org/guile-gnome/archive-2004
;;;
;;; janneke@gnu.org--2004-gnome
;;; http://lilypond.org/~janneke/{arch}/2004-gnome
;;;
(if (not (defined? '<gnome-canvas-path-def>))
    (begin
      (define-class <gnome-canvas-path-def> (<gobject>)
	(closure #:init-value (gnome-canvas-path-def-new)
		 #:init-keyword #:path-def
		 #:getter get-def #:setter set-def))
      
      (define-method (moveto (this <gnome-canvas-path-def>) x y)
	(gnome-canvas-path-def-moveto (get-def this) x y))
      (define-method (curveto (this <gnome-canvas-path-def>) x1 y1 x2 y2 x3 y3)
	(gnome-canvas-path-def-curveto (get-def this)  x1 y1 x2 y2 x3 y3))
      (define-method (lineto (this <gnome-canvas-path-def>) x y)
	(gnome-canvas-path-def-lineto (get-def this) x y))
      (define-method (closepath (this <gnome-canvas-path-def>))
	(gnome-canvas-path-def-closepath (get-def this)))
      (define-method (reset (this <gnome-canvas-path-def>))
	(gnome-canvas-path-def-reset (get-def this)))
      
      (define -set-path-def set-path-def)
      (define -get-path-def get-path-def)
      
      (define-method (set-path-def (this <gnome-canvas-shape>)
				   (def <gnome-canvas-path-def>))
	(-set-path-def this (get-def def)))
      
      (define-method (get-path-def (this <gnome-canvas-shape>))
	(make <gnome-canvas-path-def> #:path-def (-get-path-def this)))))

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

;; two beziers
(define (bezier-sandwich lst thick)
  (let* ((def (make <gnome-canvas-path-def>))
	 (bezier (make <gnome-canvas-bpath>
		   #:parent (canvas-root)
		   #:fill-color "black"
		   #:outline-color "black"
		   #:width-units thick
		   #:join-style 'round)))

    (reset def)

    ;; FIXME: LST is pre-mangled for direct ps stack usage
    ;; cl cr r l  0 1 2 3 
    ;; cr cl l r  4 5 6 7
    
    (moveto def (car (list-ref lst 3)) (- (cdr (list-ref lst 3))))
    (curveto def (car (list-ref lst 0)) (- (cdr (list-ref lst 0)))
 	     (car (list-ref lst 1)) (- (cdr (list-ref lst 1)))
 	     (car (list-ref lst 2)) (- (cdr (list-ref lst 2))))

    (lineto def (car (list-ref lst 7)) (- (cdr (list-ref lst 7))))
    (curveto def (car (list-ref lst 4)) (- (cdr (list-ref lst 4)))
 	     (car (list-ref lst 5)) (- (cdr (list-ref lst 5)))
 	     (car (list-ref lst 6)) (- (cdr (list-ref lst 6))))
    (lineto def (car (list-ref lst 3)) (- (cdr (list-ref lst 3))))

    (closepath def)
    (set-path-def bezier def)
    bezier))

(define (char font i)
  (text font (ly:font-index-to-charcode font i)))

(define (dashed-line thick on off dx dy)
  (draw-line thick 0 0 dx dy))

(define (draw-line thick x1 y1 x2 y2)
  (let* ((def (make <gnome-canvas-path-def>))
	 (props (make <gnome-canvas-bpath>
		  #:parent (canvas-root)
		  #:fill-color "black"
		  #:outline-color "black"
		  #:width-units thick)))
    (reset def)
    (moveto def x1 (- y1))
    (lineto def x2 (- y2))
    (set-path-def props def)
    props))


;; FIXME: the framework-gnome backend needs to see every item that
;; gets created.  All items created here must should be put in a group
;; that gets returned.
(define (glyph-string font postscript-font-name w-x-y-named-glyphs)
  (for-each
   (lambda (x)

     ;; UGR, glyph names not found
     (stderr "GLYPH:~S\n" (caddr x))
     (stderr "ID:~S\n" (ly:font-glyph-name-to-charcode font (caddr x)))
     (placebox (cadr x) (caddr x)
	       (make <gnome-canvas-text>
		 #:parent (canvas-root)
		 ;;#:x 0.0 #:y (if (music-font? font) 0.15 0.69)
		 #:x 0.0 #:y 0.0
		 #:anchor 'west
		 #:font (pango-font-name font)
		 #:size-points (canvas-font-size font)
		 #:size-set #t
		 #:text
		 (integer->utf-8-string
		  (ly:font-glyph-name-to-charcode font (cadddr x))))))
   w-x-y-named-glyphs))

(define (grob-cause offset grob)
  grob)


(define (named-glyph font name)
  (text font (ly:font-glyph-name-to-charcode font name)))

(define (placebox x y expr)
  (let ((item expr))
    ;;(if item
    ;; FIXME ugly hack to skip #unspecified ...
    (if (and item (not (eq? item (if #f #f))))
	(begin
	  (move item (* output-scale x) (* output-scale (- y)))
	  (affine-relative item output-scale 0 0 output-scale 0 0)
	  item)
	#f)))

(define (polygon coords blot-diameter)
  (let* ((def (make <gnome-canvas-path-def>))
	 (props (make <gnome-canvas-bpath>
		  #:parent (canvas-root)
		  #:fill-color "black"
		  #:outline-color "black"
		  #:join-style 'round)
		#:width-units blot-diameter)
	 (points (ly:list->offsets '() coords))
	 (last-point (car (last-pair points))))
    
    (reset def)
    (moveto def (car last-point) (cdr last-point))
    (for-each (lambda (x) (lineto def (car x) (cdr x))) points)
    (closepath def)
    (set-path-def props def)
    props))

(define (round-filled-box breapth width depth height blot-diameter)
  (let ((r (/ blot-diameter 2)))
    (make <gnome-canvas-rect>
      #:parent (canvas-root)
      #:x1 (- r breapth) #:y1 (- depth r) #:x2 (- width r) #:y2 (- r height)
      #:fill-color "black"
      #:outline-color "black"
      #:width-units blot-diameter
      #:join-style 'round)))

(define (text font s)
  (make <gnome-canvas-text>
    #:parent (canvas-root)
    ;;#:x 0.0 #:y 0.0
    #:x 0.0 #:y (if (music-font? font) 0.15 0.69)
    #:anchor (if (music-font? font) 'west 'south-west)
    #:font (pango-font-name font)
    #:size-points (canvas-font-size font)
    #:size-set #t
    #:text (if (integer? s)
	       (integer->utf-8-string s)
	       (string->utf-8-string s))))

(define (utf-8-string pango-font-description string)
  (make <gnome-canvas-text>
    #:parent (canvas-root)
    #:x 0.0 #:y 0.0
    #:anchor 'west
    #:font pango-font-description
    #:size-points (canvas-font-size pango-font-description)
    #:size-set #t
    #:text string))
