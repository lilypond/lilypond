;;;; output-ps.scm -- implement Scheme output interface for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;;; Note: currently misused as testbed for titles with markup, see
;;;;       input/test/title-markup.ly
;;;; 
;;;; TODO:
;;;;   * %% Papersize in (header ...)
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
	     define-origin
	     no-origin
	     start-page
	     stop-page
	     ))

(use-modules (guile)
	     (ice-9 regex)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

;;; Global vars
(define page-count 0)
(define page-number 0)

;;; helper functions, not part of output interface
(define (escape-parentheses s)
  (regexp-substitute/global #f "(^|[^\\])([\\(\\)])" s 'pre 1 "\\" 2 'post))

(define (offset-add a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

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

(define (font-command font . override-coding)
  (let* ((name (ly:font-filename font))
	 (magnify (ly:font-magnification font))
	 (coding-alist (ly:font-encoding-alist font))
	 (input-encoding (assoc-get 'input-name coding-alist))
	 (font-encoding (assoc-get 'output-name coding-alist))
	 (coding-command (if (null? override-coding)
			     (if (equal? input-encoding font-encoding)
				 #f font-encoding)
			     (car override-coding))))

    ;; FIXME:  now feta stuff has feta* input-encoding (again?)
    ;;(format (current-error-port) "FONT: ~S, ~S\n" name font-encoding)
    ;;(format (current-error-port) "INPUT: ~S\n" input-encoding)
    (if (and coding-command (equal? (substring coding-command 0 4) "feta"))
	(set! coding-command #f))

    (string-append
     "magfont" (string-encode-integer (hashq  name 1000000))
     "m" (string-encode-integer (inexact->exact (round (* 1000 magnify))))
     (if (not coding-command) "" (string-append "e" coding-command)))))

(define (define-fonts paper font-list)
  
  (define (define-font command fontname scaling)
    (string-append
     "/" command " { /" fontname " findfont "
     (ly:number->string scaling) " output-scale div scalefont } bind def\n"))

  (define (reencode-font plain encoding command)
    (let ((coding-vector (get-coding-command encoding)))
      (string-append
       plain " " coding-vector " /" command " reencode-font\n"
       "/" command "{ /" command " findfont 1 scalefont } bind def\n")))
  
  (define (guess-ps-fontname basename)
    "We do not have the FontName, try to guess is from basename."
    (cond
     ((tex-font? basename)
      ;; FIXME: we need proper Fontmap for the bluesky CM*, EC* fonts.
      ;; Only the fonts that we trace in mf/ are in our own FontMap.
      (string-append basename ".pfb"))
     (else (string-append basename ".pfa"))
     ))

  (define (font-load-command paper font)
    (let* ((specced-font-name (ly:font-name font))
	   (fontname (if specced-font-name
			 specced-font-name
			 (guess-ps-fontname (ly:font-filename font))))
	   
	   (coding-alist (ly:font-encoding-alist font))
	   (input-encoding (assoc-get 'input-name coding-alist))
	   (font-encoding (assoc-get 'output-name coding-alist))
	   (command (font-command font))
	   ;; FIXME -- see (font-command )
	   (plain (font-command font #f))
	   (designsize (ly:font-design-size font))
	   (magnification (* (ly:font-magnification font)))
	   (ops (ly:paper-lookup paper 'outputscale))
	   (scaling (* ops magnification designsize)))

      (string-append
       (define-font plain fontname scaling)
       (if (or (equal? input-encoding font-encoding)
	       ;; guh
	       (equal? font-encoding "fetaBraces")
	       (equal? font-encoding "fetaNumber")
	       (equal? font-encoding "fetaMusic")
	       (equal? font-encoding "parmesanMusic"))
	       ""
	   (reencode-font plain input-encoding command)))))
  
  (define (font-load-encoding encoding)
    (let ((filename (get-coding-filename encoding)))
      (ly:kpathsea-gulp-file filename)))

  (let* ((encoding-list (map (lambda (x)
			       (assoc-get 'input-name
					  (ly:font-encoding-alist x)))
			     font-list))
	 (encodings (uniq-list (sort-list (filter string? encoding-list)
					  string<?))))

    (string-append
     (apply string-append (map font-load-encoding encodings))
     (apply string-append
	    (map (lambda (x) (font-load-command paper x)) font-list)))))

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

(define (header creator time-stamp paper page-count- classic?)
  (set! page-count page-count-)
  (set! page-number 0)
  (string-append
   "%!PS-Adobe-3.0\n"
   "%%Creator: " creator " " time-stamp "\n"
   "%%Pages: " (number->string page-count) "\n"
   "%%PageOrder: Ascend\n"
   "%%DocumentPaperSizes: " (ly:paper-lookup paper 'papersize) "\n"
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

;; WTF is this in every backend?
(define (horizontal-line x1 x2 th)
  (draw-line th x1 0 x2 0))

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

(define (start-system origin dim)
  (string-append
   "\n" (ly:number-pair->string origin) " start-system\n"
   "{\n"
   "set-ps-scale-to-lily-scale\n"))

(define (stem breapth width depth height) 
  (string-append
   (ly:numbers->string (list breapth width depth height))
   " draw_box" ))

(define (stop-system last?)
  "} stop-system\n")

(define (symmetric-x-triangle thick w h)
  (string-append
   (ly:numbers->string (list h w thick))
   " draw_symmetric_x_triangle"))

(define (text font s)
  (let*
      
      (
       ;; ugh, we should find a better way to
       ;; extract the hsbw for /space from the font.
       
       (space-length (cdar (ly:text-dimension font "t"))) 
       (commands '())
       (add-command (lambda (x) (set! commands (cons x commands)))) )

    (string-fold
     (lambda (chr word)
       "Translate space as into moveto, group the rest in words."
       (if (and (< 0 (string-length word))
		(equal? #\space  chr))
	   (add-command 
	    (string-append "(" (ps-encoding word) ") show\n")))

       (if (equal? #\space  chr)
	   (add-command  (string-append (number->string space-length) " 0.0 rmoveto ")) )
       
       (if (equal? #\space  chr)
	   ""
	   (string-append word (make-string 1 chr))))
     ""
     (string-append  s " "))

    (string-append
     (font-command font) " setfont "
     (string-join (reverse commands)))
    ))
  
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
