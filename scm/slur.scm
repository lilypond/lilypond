
(define (attached-to-stem slur dir)
  (let* ((note-columns (ly-get-elt-pointer slur 'note-columns))
	 (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
	 (stem (ly-get-elt-pointer col 'stem)))
    (and
     (eq? col (ly-get-spanner-bound slur dir))
     stem
     (ly-get-elt-pointer stem 'heads))))


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

(define slur-extremity-rules
  (list
   (cons (lambda (slur dir)
	   ;; urg, code dup
	   (let* ((note-columns (ly-get-elt-pointer slur 'note-columns))
		  (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
		  (stem (ly-get-elt-pointer col 'stem)))
	     (and stem
		  (not (= (ly-get-elt-property slur 'direction) 
			  (ly-get-elt-property stem 'direction))))))  'head)

   (cons (lambda (slur dir)
	   ;; if attached-to-stem
	   (and (attached-to-stem slur dir)
		;; and got beam
		;; urg, code dup
		(let* ((note-columns (ly-get-elt-pointer slur 'note-columns))
		       (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
		       (stem (ly-get-elt-pointer col 'stem)))
		  (and stem
		       (ly-get-elt-pointer stem 'beam)
		       ;; and beam on same side as slur
		       (let ((beaming (ly-get-elt-property stem 'beaming)))
			 (if (pair? beaming)
			     (<= 1
				 (if (= dir -1) (car beaming) (cdr beaming)))
			     #f))))))
	 'stem)

   (cons (lambda (slur dir) (not (attached-to-stem slur dir)))  'loose-end)

   ;; default case, attach to head
   (cons (lambda (x y) #t)  'head)
   ))


;; This list defines the offsets for each type of attachment.
;; The format of each element is
;; (attachment stem-dir * attachment-dir slur-dir)
;; Different attachments have different default points:
;;
;; head: Default position is centered in X, on outer side of head Y
;; along-side-stem: Default position is on stem X, on outer side of head Y
;; stem: Default position is on stem X, at stem end Y
(define slur-extremity-offset-alist
  '(
    ((head 1 1) . (-0.25 . 0.2))
    ((head 1 -1) . (-0.25 . -0.25))
    ((head -1 1) . (-0.25 . 0.25))
    ((head -1 -1) . (-0.85 . -0.2))

    ((stem 1 1) . (0 . 0.5))
    ((stem -1 -1) . (0 . -0.5))
    ))
