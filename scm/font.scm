;;;
;;; font.scm -- implement Font stuff
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
;;;


;; corresponding properties:
;;
;;   font-series font-shape font-family font-name font-size font-points
;;
(define style-sheet-alist
  '(
    (paper16 . (
		("medium upright music feta 0 16" . "feta16")
		("medium upright music feta -1 13" . "feta13")
		("medium upright music feta -2 11" . "feta11")
		("medium upright music feta 1 20" . "feta20")
		("medium upright music feta 2 23" . "feta23")
		("medium upright orator feta-nummer 0 8" . "feta-nummer8")
		("medium upright orator feta-nummer -4 4" . "feta-nummer4")
		("medium upright roman cmr 0 8" . "cmr8")
		("medium upright roman cmr 1 10" . "cmr10")
		("bold upright roman cmbx 0 8" . "cmbx8")
		("bold upright roman cmbx 1 10" . "cmbx10")
		("medium italic roman cmbx 0 8" . "cmbx8")
		("medium italic roman cmbx 1 10" . "cmbx10")
		))
    (paper20 . (
		("medium upright music feta 0 20" . "feta20")
		("medium upright music feta -1 16" . "feta16")
		("medium upright music feta -2 13" . "feta13")
		("medium upright music feta 1 23" . "feta23")
		("medium upright music feta 2 26" . "feta26")
		("medium upright orator feta-nummer 0 10" . "feta-nummer10")
		("medium upright orator feta-nummer -4 5" . "feta-nummer5")
		("medium upright roman cmr 0 10" . "cmr10")
		("medium upright roman cmr 1 12" . "cmr12")
		("bold upright roman cmbx 0 10" . "cmbx10")
		("bold upright roman cmbx 1 12" . "cmbx12")
		("medium italic roman cmbx 0 10" . "cmbx10")
		("medium italic roman cmbx 1 12" . "cmbx12")
		))
    ))

(define (get-font-name style properties-alist)
  (let ((font-regexp
	 (let loop ((p '(font-series font-shape font-family font-name font-size font-points)) (s ""))
	   (let* ((key (if (pair? p) (car p) p))
		  (entry (assoc key properties-alist))
		  (value (if entry (cdr entry) "[^ ]+")))
	     (if (pair? (cdr p))
		 (loop (cdr p) (string-append s value " "))
		 (string-append (string-append s value))))))
	(style-sheet (cdr (assoc style style-sheet-alist))))
    ;;(display "regex: `")
    ;;(display font-regexp)
    ;;(display "'")
    ;;(newline)
    (let loop ((fonts style-sheet))
      ;;(display "font: `")
      ;;(display (caar fonts))
      ;;(display "' = ")
      ;;(display (cdar fonts))
      ;;(newline)
      (if (string-match font-regexp (caar fonts))
	  (cdar fonts)
	  (if (pair? (cdr fonts))
	      (loop (cdr fonts))
	      '())))))
	
(define markup-to-properties-alist
  '((series . font-series)
    (shape . font-shape)
    (family . font-family)
    (name . font-name)
    (size . font-size)
    (point . font-point)))

(define markup-abbrev-to-properties-alist
  '((rows . (align . 0))
    (lines . (align . 1))
    (roman . (font-family . "roman"))
    (music . (font-family . "music"))
    (bold . (font-series . "bold"))
    (italic . (font-shape . "italic"))))
    
(define (markup-to-properties markup)
  ;;(display "markup: ")
  ;;(display markup)
  ;;(newline)
  (if (pair? markup)
      (cons (cdr (assoc (car markup) markup-to-properties-alist)) (cdr markup))
      (cdr (assoc markup markup-abbrev-to-properties-alist))))
	
