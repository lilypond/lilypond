; lily.scm -- implement Scheme output routines for TeX and PostScript
;
;  source file of the GNU LilyPond music typesetter
; 
; (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>


;(debug-enable 'backtrace)

;;; library funtions
(define
  (xnumbers->string l)
  (string-append 
   (map (lambda (n) (string-append (number->string n ) " ")) l)))

(define (reduce operator list)
      (if (null? (cdr list)) (car list)
	  (operator (car list) (reduce operator (cdr list)))
	  )
      )


(define (glue-2-strings a b) (string-append a " " b))

(define
  (numbers->string l)
  (reduce glue-2-strings (map number->string l)))

(define (chop-decimal x) (if (< (abs x) 0.001) 0.0 x))

(define (number->octal-string x)
  (let* ((n (inexact->exact x))
         (n64 (quotient n 64))
         (n8 (quotient (- n (* n64 64)) 8)))
    (string-append
     (number->string n64)
     (number->string n8)
     (number->string (remainder (- n (+ (* n64 64) (* n8 8))) 8)))))

(define (inexact->string x radix)
  (let ((n (inexact->exact x)))
    (number->string n radix)))


(define
  (control->string c)
  (string-append
   (string-append (number->string (car c)) " ")
   (string-append (number->string (cadr c)) " ")))


(define
  (font i)
  (string-append
   "font"
   (make-string 1 (integer->char (+ (char->integer #\A) i)))
   ))



(define (scm-scm action-name)
  1)

(define security-paranoia #f)


;; See documentation of Item::visibility_lambda_
(define (postbreak_only_visibility d) (if (= d 1) '(#f . #f) '(#t . #t)))
(define (spanbar_non_postbreak_visibility d) (if (= d -1) '(#t . #t) '(#f . #f)))

(define (non_postbreak_visibility d) (if (= d 1) '(#t . #t) '(#f . #f)))
(define (non_prebreak_visibility d) (if (= d -1) '(#t . #t) '(#f . #f)))


;; Score_span_bars are only visible at start of line
;; i.e. if break_dir == RIGHT == 1
(define Span_bar_engraver_visibility non_postbreak_visibility)
(define mark-visibility non_prebreak_visibility)
(define Span_score_bar_engraver_visibility postbreak_only_visibility)
(define Piano_bar_engraver_visibility postbreak_only_visibility)
(define Staff_group_bar_engraver_visibility postbreak_only_visibility)

;; Spacing constants for prefatory matter.
;;
;; rules for this are complicated. See [Wanske] page 126 -- 134
;;
;;
(define space-alist
 '(
   (("Clef_item" "Key_item") .  2.5)
   (("Key_item" "Time_signature") . 2.5)
   (("Clef_item"  "Time_signature") . 2.75)
   (("Staff_bar" "Clef_item") .  1.0)
   (("Clef_item"  "Staff_bar") . 3.7)
   (("Time_signature" "Staff_bar") . 2.0)
   (("Key_item"  "Staff_bar") . 2.5)
   (("Span_bar" "Clef_item") .  1.0)
   (("Clef_item"  "Span_bar") . 3.7)
   (("Time_signature" "Span_bar") . 2.0)
   (("Key_item"  "Span_bar") . 2.5)
   (("Staff_bar" "Time_signature") . 1.0)
   )
 
 )

(define (break-align-spacer this next)
  (let ((entry (assoc `(,this ,next) space-alist)))
    (if entry
	(cdr entry)
	(begin (ly-warn (string-append "Unknown spacing pair " this ", " next))
	       0.0))))
  
	

;;;;;;;; TeX

(define cmr-alist 
  '(("bold" . "cmbx") 
    ("dynamic" . "feta-din") 
    ("finger" . "feta-nummer") 
    ("typewriter" . "cmtt") 
    ("italic" . "cmti") 
    ("roman" . "cmr") 
    ("large" . "cmbx") 
    ("Large" . "cmbx") 
    ("mark" . "feta-nummer") 
    ("number" . "feta-nummer") 
    ("volta" . "feta-nummer"))
)

(define script-alist '())
(define (articulation-to-scriptdef a)
  (assoc a script-alist)
  )

;; Map style names to TeX font names.  Return false if 
;; no font name found. 
(define (style-to-cmr s)
  (assoc s cmr-alist )
  )


(define (tex-scm action-name)

  (define (unknown) 
    "%\n\\unknown%\n")

  (define font-alist '())
  (define font-count 0)
  (define current-font "")
  (define (clear-fontcache)
    (begin
      (set! font-alist '())
      (set! font-count 0)
      (set! current-font "")))
  
  (define (cached-fontname i)
    (string-append
     "\\lilyfont"
     (make-string 1 (integer->char (+ 65 i)))))
    
  (define (select-font font-name)
      (if (not (equal? font-name current-font))
	  (begin
	    (set! current-font font-name)
	    (define font-cmd (assoc font-name font-alist))
	    (if (eq? font-cmd #f)
		(begin
		  (set! font-cmd (cached-fontname font-count))
		  (set! font-alist (acons font-name font-cmd font-alist))
		  (set! font-count (+ 1 font-count))
		  (if (equal? font-name "")
		      (error "Empty fontname -- SELECT-FONT"))
		  (string-append "\\font" font-cmd "=" font-name font-cmd))
		(cdr font-cmd)))
	  ""				;no switch needed
	  ))
  
  (define (beam width slope thick)
    (embedded-ps ((ps-scm 'beam) width slope thick)))

  (define (bracket h)
    (embedded-ps ((ps-scm 'bracket) h)))

  (define (dashed-slur thick dash l)
    (embedded-ps ((ps-scm 'dashed-slur)  thick dash l)))

  (define (crescendo w h cont)
    (embedded-ps ((ps-scm 'crescendo) w h cont)))

  (define (char i)
    (string-append "\\show{" (inexact->string i 10) "}"))
  
  (define (decrescendo w h cont)
    (embedded-ps ((ps-scm 'decrescendo) w h cont)))

  (define (embedded-ps s)
    (string-append "\\embeddedps{" s "}"))

  (define (end-output) 
    "\n\\EndLilyPondOutput")
  
  (define (experimental-on)
    "\\turnOnExperimentalFeatures")

  (define (font-switch i)
    (string-append
     "\\" (font i) "\n"))

  (define (font-def i s)
    (string-append
     "\\font" (font-switch i) "=" s "\n"))

  (define (generalmeter num den)
    (string-append 
     "\\generalmeter{" (number->string (inexact->exact num)) "}{" (number->string (inexact->exact den)) "}"))

  (define (header-end) "\\turnOnPostScript")

  (define (header creator generate) 
    (string-append
     "%created by: " creator generate "\n"))

  (define (invoke-char s i)
    (string-append 
     "\n\\" s "{" (inexact->string i 10) "}" ))

  (define (invoke-dim1 s d)
    (string-append
     "\n\\" s "{" (number->dim d) "}"))
  (define (pt->sp x)
    (* 65536 x))
  
  ;;
  ;; need to do something to make this really safe.
  ;;
  (if security-paranoia
      (define (output-tex-string s)
	(regexp-substitute/global #f "\\\\" s 'pre "$\\backslash$" 'post))
      (define (output-tex-string s)    s))

  (define (lily-def key val)
    (string-append
     "\\def\\" (output-tex-string key) "{" (output-tex-string val) "}\n"))

  (define (number->dim x)
    (string-append 
     (number->string  (chop-decimal x)) " pt "))

  (define (placebox x y s) 
    (string-append 
     "\\placebox{"
     (number->dim y) "}{" (number->dim x) "}{" s "}"))

  (define (pianobrace y)
    (define step 1.0)
    (define minht (* 2 mudelapaperstaffheight))
    (define maxht (* 7 minht))
    (string-append
     "{\\bracefont " (char (max
			    0
			    (/  (- (min y (- maxht step)) minht) step))) "}"))



  (define (rulesym h w) 
    (string-append 
     "\\vrule height " (number->dim (/ h 2))
     " depth " (number->dim (/ h 2))
     " width " (number->dim w)
     )
    )

  (define (bezier-sandwich l)
    (embedded-ps ((ps-scm 'bezier-sandwich) l)))


  (define (start-line ht)
    (begin
      (clear-fontcache)
      (string-append"\\vbox to " (number->dim ht) "{\\hbox{%\n"))
    )
  (define (stop-line) 
    "}\\vss}\\interscoreline")
  (define (stop-last-line)
    "}\\vss}")
  (define (filledbox breapth width depth height) 
    (string-append 
     "\\kern" (number->dim (- breapth))
     "\\vrule width " (number->dim (+ breapth width))
     "depth " (number->dim depth)
     "height " (number->dim height) " "))



  (define (text s)
    (string-append "\\hbox{" (output-tex-string s) "}"))
  
  (define (tuplet ht dx dy thick dir)
    (embedded-ps ((ps-scm 'tuplet) ht dx dy thick dir)))

  (define (volta w thick last)
    (embedded-ps ((ps-scm 'volta) w thick last)))

  ;; TeX
  ;; The procedures listed below form the public interface of TeX-scm.
  ;; (should merge the 2 lists)
  (cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define beam ,beam)
	    (define bezier-sandwich ,bezier-sandwich)
	    (define bracket ,bracket)
	    (define char ,char)
	    (define crescendo ,crescendo)
	    (define dashed-slur ,dashed-slur) 
	    (define decrescendo ,decrescendo) 
	    (define end-output ,end-output)
	    (define experimental-on ,experimental-on)
	    (define filledbox ,filledbox)
	    (define font-def ,font-def)
	    (define font-switch ,font-switch)
	    (define generalmeter ,generalmeter)
	    (define header-end ,header-end)
	    (define lily-def ,lily-def)
	    (define header ,header) 
	    (define invoke-char ,invoke-char) 
	    (define invoke-dim1 ,invoke-dim1)
	    (define pianobrace ,pianobrace)
	    (define placebox ,placebox)
	    (define rulesym ,rulesym)
	    (define select-font ,select-font)
	    (define start-line ,start-line)
	    (define stop-line ,stop-line)
	    (define stop-last-line ,stop-last-line)
	    (define text ,text)
	    (define tuplet ,tuplet)
	    (define volta ,volta)
	    ))

	((eq? action-name 'beam) beam)
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'crescendo) crescendo)
	((eq? action-name 'dashed-slur) dashed-slur) 
	((eq? action-name 'decrescendo) decrescendo) 
	((eq? action-name 'end-output) end-output)
	((eq? action-name 'experimental-on) experimental-on)
	((eq? action-name 'font-def) font-def)
	((eq? action-name 'font-switch) font-switch)
	((eq? action-name 'generalmeter) generalmeter)
	((eq? action-name 'header-end) header-end)
	((eq? action-name 'lily-def) lily-def)
	((eq? action-name 'header) header) 
	((eq? action-name 'invoke-char) invoke-char) 
	((eq? action-name 'invoke-dim1) invoke-dim1)
	((eq? action-name 'placebox) placebox)
	((eq? action-name 'rulesym) rulesym)
	((eq? action-name 'bezier-sandwich) bezier-sandwich)
	((eq? action-name 'start-line) start-line)
	((eq? action-name 'stem) stem)
	((eq? action-name 'stop-line) stop-line)
	((eq? action-name 'stop-last-line) stop-last-line)
	((eq? action-name 'volta) volta)
	(else (error "unknown tag -- PS-TEX " action-name))
	)
  )

;;;;;;;;;;;; PS
(define (ps-scm action-name)


  ;; alist containing fontname -> fontcommand assoc (both strings)
  (define font-alist '())
  (define font-count 0)
  (define current-font "")
  (define (clear-fontcache)
    (begin
      (set! font-alist '())
      (set! font-count 0)
      (set! current-font "")))
  
  (define (cached-fontname i)
    (string-append
     " lilyfont"
     (make-string 1 (integer->char (+ 65 i)))))

  (define (select-font font-name)
      (if (not (equal? font-name current-font))
	  (begin
	    (set! current-font font-name)
	    (define font-cmd (assoc font-name font-alist))
	    (if (eq? font-cmd #f)
		(begin
		  (set! font-cmd (cached-fontname font-count))
		  (set! font-alist (acons font-name font-cmd font-alist))
		  (set! font-count (+ 1 font-count))
		  (string-append "\n/" font-cmd " {/"
				 font-name
				 " findfont 12 scalefont setfont} bind def \n"
				 font-cmd " \n"))
		(string-append (cdr font-cmd) " ")))
	  ; font-name == current-font no switch needed
	  ""				
	  ))
		  
  (define (beam width slope thick)
    (string-append
     (numbers->string (list width slope thick)) " draw_beam " ))

  (define (bracket h)
    (invoke-dim1 " draw_bracket" h))

  (define (char i)
    (invoke-char " show" i))

  (define (crescendo w h cont)
    (string-append 
     (numbers->string (list w h (inexact->exact cont)))
     " draw_crescendo"))

  (define (dashed-slur thick dash l)
    (string-append 
     (apply string-append (map control->string l)) 
     (number->string thick) 
     " [ "
     (if (> 1 dash) (number->string (- (* thick dash) thick)) "0") " "
     (number->string (* 2 thick))
     " ] 0 draw_dashed_slur"))

  (define (decrescendo w h cont)
    (string-append 
     (numbers->string (list w h (inexact->exact cont)))
     " draw_decrescendo"))


  (define (end-output)
    "\nshowpage\n")
  
  (define (experimental-on) "")
  
  (define (filledbox breapth width depth height) 
    (string-append (numbers->string (list breapth width depth height))
		   " draw_box" ))

  ;; obsolete?
  (define (font-def i s)
    (string-append
     "\n/" (font i) " {/" 
     (substring s 0 (- (string-length s) 4))
     " findfont 12 scalefont setfont} bind def \n"))

  (define (font-switch i)
    (string-append (font i) " "))

  (define (generalmeter num den)
    (string-append (number->string (inexact->exact num)) " " (number->string (inexact->exact den)) " generalmeter "))

  (define (header-end) "")
  (define (lily-def key val)
    (string-append
     "/" key " {" val "} bind def\n"))

  (define (header creator generate) 
    (string-append
     "%!PS-Adobe-3.0\n"
     "%%Creator: " creator generate "\n"))
  
  (define (invoke-char s i)
    (string-append 
     "(\\" (inexact->string i 8) ") " s " " ))
  
  (define (invoke-dim1 s d) 
    (string-append
     (number->string (* d  (/ 72.27 72))) " " s ))

  (define (placebox x y s) 
    (string-append 
     (number->string x) " " (number->string y) " {" s "} placebox "))
  (define (pianobrace y)
    ""
    )

  (define (rulesym x y) 
    (string-append 
     (number->string x) " "
     (number->string y) " "
     " rulesym"))

  (define (bezier-sandwich l)
    (string-append 
     (apply string-append (map control->string l)) 
     " draw_bezier_sandwich"))

  (define (start-line height)
    (begin
      (clear-fontcache)
      "\nstart_line {\n"))
  
  (define (stem breapth width depth height) 
    (string-append (numbers->string (list breapth width depth height))
		   " draw_box" ))

  (define (stop-line)
      "}\nstop_line\n")

  (define (text s)
    (string-append "(" s ") show  "))


  (define (volta w thick last)
    (string-append 
     (numbers->string (list w thick (inexact->exact last)))
     " draw_volta"))

  (define (tuplet ht dx dy thick dir)
    (string-append 
     (numbers->string (list ht dx dy thick (inexact->exact dir)))
     " draw_tuplet"))


  (define (unknown) 
    "\n unknown\n")


  ;; PS
  (cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define beam ,beam)
	    (define tuplet ,tuplet)
	    (define bracket ,bracket)
	    (define char ,char)
	    (define crescendo ,crescendo)
	    (define volta ,volta)
	    (define bezier-sandwich ,bezier-sandwich)
	    (define dashed-slur ,dashed-slur) 
	    (define decrescendo ,decrescendo) 
	    (define end-output ,end-output)
	    (define experimental-on ,experimental-on)
	    (define filledbox ,filledbox)
	    (define font-def ,font-def)
	    (define font-switch ,font-switch)
	    (define generalmeter ,generalmeter)
	    (define pianobrace ,pianobrace)
	    (define header-end ,header-end)
	    (define lily-def ,lily-def)
	    (define header ,header) 
	    (define invoke-char ,invoke-char) 
	    (define invoke-dim1 ,invoke-dim1)
	    (define placebox ,placebox)
	    (define rulesym ,rulesym)
	    (define select-font ,select-font)
	    (define start-line ,start-line)
	    (define stem ,stem)
	    (define stop-line ,stop-line)
	    (define stop-last-line ,stop-line)
	    (define text ,text)
	    ))
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'beam) beam)
	((eq? action-name 'bezier-sandwich) bezier-sandwich)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'char) char)
	((eq? action-name 'crescendo) crescendo)
	((eq? action-name 'dashed-slur) dashed-slur) 
	((eq? action-name 'decrescendo) decrescendo)
	((eq? action-name 'experimental-on) experimental-on)
	((eq? action-name 'filledbox) filledbox)
	((eq? action-name 'select-font) select-font)
	((eq? action-name 'volta) volta)
	(else (error "unknown tag -- PS-SCM " action-name))
	)
  )

					;
; Russ McManus, <mcmanus@IDT.NET>  
; 
; I use the following, which should definitely be provided somewhere
; in guile, but isn't, AFAIK:
; 
; 

(define (hash-table-for-each fn ht)
  (do ((i 0 (+ 1 i)))
      ((= i (vector-length ht)))
    (do ((alist (vector-ref ht i) (cdr alist)))
	((null? alist) #t)
      (fn (car (car alist)) (cdr (car alist))))))

(define (hash-table-map fn ht)
  (do ((i 0 (+ 1 i))
       (ret-ls '()))
      ((= i (vector-length ht)) (reverse ret-ls))
    (do ((alist (vector-ref ht i) (cdr alist)))
	((null? alist) #t)
      (set! ret-ls (cons (fn (car (car alist)) (cdr (car alist))) ret-ls)))))

