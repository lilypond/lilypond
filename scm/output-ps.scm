;;;; output-ps.scm -- implement Scheme output routines for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;;; Note: currently misused as testbed for titles with markup, see
;;;;       input/test/title-markup.ly
;;;; 
;;;; TODO:
;;;;   * papersize in header
;;;;   * special characters, encoding.
;;;;     + implement encoding switch (switches?  input/output??),
;;;;     + move encoding definitions to ENCODING.ps files, or
;;;;     + find out which program's PS(?) encoding code we can use?
;;;;   * text setting, kerning.
;;;;   * document output-interface

(debug-enable 'backtrace)

(define-module (scm output-ps)
  #:re-export (quote)
  #:export (define-fonts
	     unknown
	     output-paper-def
	     output-scopes
	     select-font
	     blank
	     dot
	     beam
	     bracket
	     dashed-slur
	     char
	     dashed-line
	     zigzag-line
	     symmetric-x-triangle
	     ez-ball
	     comment
	     end-output
	     experimental-on
	     repeat-slash
	     header-end
	     header
	     placebox
	     bezier-sandwich
	     start-system
	     stop-system
	     stop-last-system
	     horizontal-line
	     filledbox
	     round-filled-box
	     text
	     tuplet
	     polygon
	     draw-line
	     between-system-string
	     define-origin
	     no-origin
	     start-page
	     stop-page
	     )
)
(use-modules (guile)
	     (ice-9 regex)
	     (srfi srfi-13)
	     (lily))

;;; Global vars
;; alist containing fontname -> fontcommand assoc (both strings)
(define page-count 0)
(define page-number 0)

;; /lilypondpaperoutputscale 1.75729901757299 def
;;/lily-output-units 2.83464  def  %% milimeter
;;/output-scale lilypondpaperoutputscale lily-output-units mul def
;;
;; output-scale = 1.75729901757299 * 2.83464 = 4.9813100871731003736

(define OUTPUT-SCALE 4.98)
(define TOP-MARGIN 0)

;;; helper functions, not part of output interface
(define (escape-parentheses s)
  (regexp-substitute/global #f "(^|[^\\])([\\(\\)])" s 'pre 1 "\\" 2 'post))

(define (offset-add a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

;; WIP
(define font-encoding-alist
  '(("ecrm12" . "ISOLatin1Encoding")
    ("ecmb12" . "ISOLatin1Encoding")))
		 
(define (ps-encoding text)
  (escape-parentheses text))

;; FIXME: lily-def
(define (ps-string-def prefix key val)
  (string-append "/" prefix (symbol->string key) " ("
		 (escape-parentheses val)
		 ") def\n"))

(define (ps-number-def prefix key val)
  (let ((s (if (integer? val)
	       (ly:number->string val)
	       (ly:number->string (exact->inexact val)))))
    (string-append "/" prefix (symbol->string key) " " s " def\n")))

(define (tex-font? fontname)
  (equal? (substring fontname 0 2) "cm"))


;;;
;;; Lily output interface, PostScript implementation --- cleanup and docme
;;;

;;; Output-interface functions
(define (beam width slope thick blot)
  (string-append
   (ly:numbers->string (list slope width thick blot)) " draw_beam" ))

;; two beziers
(define (bezier-sandwich l thick)
  (string-append 
   (string-join (map ly:number-pair->string l) " ")
   " "
   (ly:number->string thick)
   " draw_bezier_sandwich"))

(define (bracket arch_angle arch_width arch_height  height arch_thick thick)
  (string-append
   (ly:numbers->string
    (list arch_angle arch_width arch_height height arch_thick thick))
   " draw_bracket"))

(define (char font i)
  (string-append 
    (font-command font) " setfont " 
   "(\\" (ly:inexact->string i 8) ") show" ))

(define (comment s)
  (string-append "% " s "\n"))

(define (dashed-line thick on off dx dy)
  (string-append 
   (ly:number->string dx) " "
   (ly:number->string dy) " "
   (ly:number->string thick)
   " [ "
   (ly:number->string on) " "
   (ly:number->string off)
   " ] 0 draw_dashed_line"))

;; what the heck is this interface ?
(define (dashed-slur thick dash l)
  (string-append 
   (string-join (map ly:number-pair->string l) " ")
   " "
   (ly:number->string thick) 
   " [ "
   (ly:number->string dash)
   " "
   ;;UGH.  10 ?
   (ly:number->string (* 10 thick))
   " ] 0 draw_dashed_slur"))

(define (font-command font)
  (string-append
   "magfont"
   (string-encode-integer
    (hashq (ly:font-name font) 1000000))
   "m"
   (string-encode-integer
    (inexact->exact (round (* 1000 (ly:font-magnification font)))))))

(define (define-fonts paper font-list)

  (define (fontname->designsize fontname)
    (let ((i (string-index fontname char-numeric?)))
      (string->number (substring fontname i))))
  
  (define (define-font command fontname scaling)
    (string-append
     "/" command " { /" fontname " findfont "
     ;; FIXME
     (ly:number->string scaling) " output-scale div scalefont } bind def\n"))
     ;;(ly:number->string scaling) " scalefont } bind def\n"))

  (define (reencode-font raw encoding command)
    (string-append
     raw " " encoding " /" command " reencode-font\n"
     "/" command "{ /" command " findfont 1 scalefont } bind def\n"))
	  
  ;; frobnicate NAME to jibe with external definitions.
  (define (possibly-mangle-fontname fontname)
    (cond
     ((tex-font? fontname)
      ;; FIXME: we need proper Fontmap for CM fonts, like so:
      ;; /CMR10 (cmr10.pfb); 
      ;; (string-upcase fontname)
      (string-append fontname ".pfb"))
     ((or (equal? (substring fontname 0 4) "feta")
	  (equal? (substring fontname 0 4) "parm"))
      (regexp-substitute/global
       #f "(feta|parmesan)([a-z-]*)([0-9]+)"
       fontname 'pre "GNU-LilyPond-" 1 2 "-" 3 'post))
     (else fontname)))
			 
  (define (font-load-command paper font)
    (let* ((command (font-command font))
	   (fontname (ly:font-name font))
	   (mangled (possibly-mangle-fontname fontname))
	   (encoding (assoc-get fontname font-encoding-alist))
	   (designsize (ly:font-design-size font))
	   (magnification (* (ly:font-magnification font)))
	   (ops (ly:paper-lookup paper 'outputscale))
	   (scaling (* ops magnification designsize)) )

      (if
       #f
       (begin
	 (newline)
	 (format (current-error-port) "fontname ~S\n" fontname)
	 (format (current-error-port) "command ~S\n" command)
	 (format (current-error-port) "mangled ~S\n" mangled)
	 (format (current-error-port) "designsize ~S\n" designsize)
	 (format (current-error-port) "foo-design ~S\n" foo-design)
	 (format (current-error-port) "magnification ~S\n" magnification)
	 (format (current-error-port) "ops ~S\n" ops)
	 (format (current-error-port) "scaling ~S\n" scaling)))
      
      (if encoding
	  ;; FIXME: should rather tag encoded font
	  (let ((raw (string-append command "-raw")))
	    (string-append
	     (define-font raw mangled scaling)
	     (reencode-font raw encoding command)))
	  (define-font command mangled scaling))))
  
  (apply string-append
	 (map (lambda (x) (font-load-command paper x)) font-list)))

(define (define-origin file line col) "")

(define (dot x y radius)
  (string-append
   " "
   (ly:numbers->string
    (list x y radius)) " draw_dot"))

(define (draw-line thick x1 y1 x2 y2)
  (string-append 
   "1 setlinecap 1 setlinejoin "
   (ly:number->string thick) " setlinewidth "
   (ly:number->string x1) " "
   (ly:number->string y1) " moveto "
   (ly:number->string x2) " "
   (ly:number->string y2) " lineto stroke"))

(define (end-output)
  "\nend-lilypond-output\n")

(define (ez-ball ch letter-col ball-col)
  (string-append
   " (" ch ") "
   (ly:numbers->string (list letter-col ball-col))
   " /Helvetica-Bold " ;; ugh
   " draw_ez_ball"))

(define (filledbox breapth width depth height) 
  (string-append (ly:numbers->string (list breapth width depth height))
		 " draw_box"))

(define (header creator time-stamp page-count-)
  (set! page-count page-count-)
  (set! page-number 0)
  (string-append
   "%!PS-Adobe-3.0\n"
   "%%Creator: " creator " " time-stamp "\n"
   "%%Pages: " (number->string page-count) "\n"
   "%%PageOrder: Ascend\n"
   ;; FIXME: TODO get from paper
   ;; "%%DocumentPaperSizes: a6\n"
   ;;(string-append "GNU LilyPond (" (lilypond-version) "), ")
   ;;	   (strftime "%c" (localtime (current-time))))
   ;; FIXME: duplicated in every backend
   (ps-string-def
    "lilypond" 'tagline
    (string-append "Engraved by LilyPond (version " (lilypond-version) ")"))))

(define (header-end)
  (string-append
   (ly:gulp-file "lilyponddefs.ps")
   (ly:gulp-file "music-drawing-routines.ps")))

(define (horizontal-line x1 x2 th)
  (draw-line th x1  0 x2 0))

(define (lily-def key val)
  (let ((prefix "lilypondpaper"))
    (if (string=?
	 (substring key 0 (min (string-length prefix) (string-length key)))
	 prefix)
	(string-append "/" key " {" val "} bind def\n")
	(string-append "/" key " (" val ") def\n"))))

(define (no-origin) "")

;; FIXME: duplictates output-scopes, duplicated in other backends
;; FIXME: silly interface name
(define (output-paper-def pd)
  (let ((prefix "lilypondpaper"))
    
    (define (scope-entry->string key var)
      (if (variable-bound? var)
	  (let ((val (variable-ref var)))
	    (cond
	     ((string? val) (ps-string-def prefix key val))
	     ((number? val) (ps-number-def prefix key val))
	     (else "")))
	  ""))
      
    (apply
     string-append
     (module-map scope-entry->string (ly:output-def-scope pd)))))

;; FIXME: duplicated in other output backends
;; FIXME: silly interface name
(define (output-scopes paper scopes fields basename)
  (let ((prefix "lilypond"))

    ;; FIXME: duplicates output-paper's scope-entry->string, mostly
    (define (scope-entry->string key var)
      (if (variable-bound? var)
	  (let ((val (variable-ref var)))
	    (if (and (memq key fields) (string? val))
		(header-to-file basename key val))
	    (cond
	     ((string? val) (ps-string-def prefix key val))
	     ((number? val) (ps-number-def prefix key val))
	     (else "")))
	  ""))
    
    (define (output-scope scope)
      (apply string-append (module-map scope-entry->string scope)))

    (string-append (apply string-append (map output-scope scopes)))))

;; hmm, looks like recursing call is always last statement, does guile
;; think so too?
(define (output-stencil port expr offset)
  (if (pair? expr)
      (let ((head (car expr)))
	(cond
	 ((ly:input-location? head)
	  (display (apply define-origin (ly:input-location head)) port)
	  (output-stencil port (cadr expr) offset))
	 ((eq? head 'no-origin)
	  (display (expression->string head) port)
	  (output-stencil port (cadr expr) offset))
	 ((eq? head 'translate-stencil)
	  (output-stencil port (caddr expr) (offset-add offset (cadr expr))))
	 ((eq? head 'combine-stencil)
	  (output-stencil port (cadr expr) offset)
	  (output-stencil port (caddr expr) offset))
	 (else
	  (display (placebox (car offset) (cdr offset)
			     (expression->string expr)) port))))))

(define (placebox x y s) 
  (string-append 
   (ly:number->string x) " " (ly:number->string y) " { " s " } place-box\n"))

(define (polygon points blotdiameter)
  (string-append
   (ly:numbers->string points) " "
   (ly:number->string (/ (length points) 2)) " "
   (ly:number->string blotdiameter)
   " draw_polygon"))

(define (repeat-slash wid slope thick)
  (string-append
   (ly:numbers->string (list wid slope thick))
   " draw_repeat_slash"))

(define (round-filled-box x y width height blotdiam)
   (string-append
    (ly:numbers->string
     (list x y width height blotdiam)) " draw_round_box"))

(define (new-start-system origin dim)
  (string-append
   "\n" (ly:number-pair->string origin) " start-system\n"
   "{\n"
   "set-ps-scale-to-lily-scale\n"))

(define (stem breapth width depth height) 
  (string-append
   (ly:numbers->string (list breapth width depth height))
   " draw_box" ))

(define (stop-system)
  "} stop-system\n")

(define stop-last-system stop-system)

(define (symmetric-x-triangle thick w h)
  (string-append
   (ly:numbers->string (list h w thick))
   " draw_symmetric_x_triangle"))

(define (text font s)
  
;;  (string-append "(" (escape-parentheses s) ") show "))
  (string-append

   (font-command font) " setfont " 
   "(" (ps-encoding s) ") show"))

(define (unknown) 
  "\n unknown\n")

(define (zigzag-line centre? zzw zzh thick dx dy)
  (string-append
    (if centre? "true" "false") " "
    (ly:number->string zzw) " "
    (ly:number->string zzh) " "
    (ly:number->string thick) " "
    "0 0 "
    (ly:number->string dx) " "
    (ly:number->string dy)
    " draw_zigzag_line"))

(define (start-page)
  (set! page-number (+ page-number 1))
  (string-append
   "%%Page: " (number->string page-number) " " (number->string page-count) "\n"
  "start-page\n"))

(define (stop-page last?)
  (if last?
      "\nstop-last-page\n"
      "\nstop-page\n"))
