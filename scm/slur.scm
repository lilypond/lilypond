;;;
;;; slur.scm -- Slur scheme stuff
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
;;;

(define (attached-to-stem slur dir)
  (let* ((note-columns (ly-get-elt-property slur 'note-columns))
	 (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
	 (stem (ly-get-elt-property col 'stem)))
    (and
     (eq? col (ly-get-spanner-bound slur dir))
     stem
     (ly-get-elt-property stem 'heads))))


;; Slur-extremity-rules is a list of rules.  Each rule is a pair 
;; (fuction . attachment), where function takes two arguments,
;; the slur and the direction of the attachment.
;;
;; The rules are tried starting from the car of this list.  If the
;; function part (car) evaluates to #t, the corresponding
;; attachment (cdr) is used for the slur's dir.  Otherwise, the next
;; rule is tried.
;;
;; Currently, we have attachments:
;;
;;    'head 'along-side-stem 'stem 'loose-end
;;

(define default-slur-extremity-rules
  (list

   ;; (cons (lambda (slur dir) (begin (display "before sanity check") (newline))#f) #f)

   ;; urg: don't crash on a slur without note-columns
   (cons (lambda (slur dir)
	   (< (length (ly-get-elt-property slur 'note-columns)) 1)) 'head)

   ;; (cons (lambda (slur dir) (begin (display "before loose-end") (newline))#f) #f)
   (cons (lambda (slur dir) (not (attached-to-stem slur dir)))  'loose-end)

   ;; (cons (lambda (slur dir) (begin (display "before head") (newline))#f) #f)

   (cons (lambda (slur dir)
	   ;; urg, code dup
	   (let* ((note-columns (ly-get-elt-property slur 'note-columns))
		  (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
		  (stem (ly-get-elt-property col 'stem)))
	     (and stem
		  (not (= (ly-get-elt-property slur 'direction) 
			  (ly-get-elt-property stem 'direction))))))  'head)

   ;; (cons (lambda (slur dir) (begin (display "before stem") (newline))#f) #f)

   (cons (lambda (slur dir)
	   ;; if attached-to-stem
	   (and (attached-to-stem slur dir)
		;; and got beam
		;; urg, code dup
		(let* ((note-columns (ly-get-elt-property slur 'note-columns))
		       (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
		       (stem (ly-get-elt-property col 'stem)))
		  (and stem
		       (ly-get-elt-property stem 'beam)
		       ;; and beam on same side as slur
		       (let ((beaming (ly-get-elt-property stem 'beaming)))
			 (if (pair? beaming)
			     (<= 1
				 (if (= dir -1) (car beaming) (cdr beaming)))
			     #f))))))
	 'stem)

   ;;(cons (lambda (slur dir) (begin (display "before loose-end") (newline))#f) #f)
   (cons (lambda (slur dir) (not (attached-to-stem slur dir)))  'loose-end)
   ;; (cons (lambda (slur dir) (begin (display "after loose-end") (newline))#f) #f)

   ;; default case, attach to head
   (cons (lambda (x y) #t)  'head)
   ))


;; This list defines the offsets for each type of attachment.
;; The format of each element is
;; (attachment stem-dir*dir slur-dir*dir)
;; Different attachments have different default points:
;;
;; head: Default position is centered in X, on outer side of head Y
;; along-side-stem: Default position is on stem X, on outer side of head Y
;; stem: Default position is on stem X, at stem end Y
(define default-slur-extremity-offset-alist
  '(
    ((head 1 1) . (-0.25 . 0.25))
    ((head 1 -1) . (-0.25 . -0.25))
    ((head -1 1) . (-0.25 . 0.25))
    ((head -1 -1) . (-0.85 . -0.25))

    ((stem 1 1) . (0 . 0.5))
    ((stem -1 -1) . (0 . -0.5))

    ((loose-end 1 1) . (-0.4 . 0))
    ((loose-end 1 -1) . (-0.4 . 0))
    ((loose-end -1 -1) . (-4 . 0))
    ((loose-end -1 1) . (-4 . 0))
    ))


(define default-basic-slur-properties
  `(
   (molecule-callback . ,Slur::brew_molecule)
   (thickness . 1.2)		
   (spacing-procedure . ,Slur::set_spacing_rods)		
   (minimum-length . 1.5)
   (after-line-breaking-callback . ,Slur::after_line_breaking)
   (extremity-rules . ,default-slur-extremity-rules)
   (extremity-offset-alist . ,default-slur-extremity-offset-alist)
   (de-uglify-parameters . ( 1.5  0.8  -2.0))
   (details . ((height-limit . 2.0) (ratio . 0.333) (force-blowfit . 0.5)
   	(bezier-pct-c0 . -0.2) (bezier-pct-c3 . 0.000006)
	(bezier-pct-out-max . 0.8) (bezier-pct-in-max . 1.2)
	(bezier-area-steps . 1.0)))
   (beautiful . 0.5)
   (y-free . 0.75)
   (attachment-offset . ((0 . 0) . (0 . 0)))
   (slope-limit . 0.8)
   (meta . ,(element-description "Slur" slur-interface))
   )
  )
