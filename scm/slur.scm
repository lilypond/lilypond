
(define (attached-to-stem slur dir)
  (let* ((note-columns (get-pointer slur 'note-columns))
	 (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
	 (stem (get-pointer col 'stem)))
    (and
     (eq? col (get-bound slur dir))
     stem
     (get-pointer stem 'heads))))

(define slur-extremity-rules
  '(
    ((lambda (slur dir)
       ;; urg, code dup
       (let* ((note-columns (get-pointer slur 'note-columns))
	 (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
	 (stem (get-pointer col 'stem)))
	 (and stem
	      (not (= (get-property slur 'direction) 
		      (get-property stem 'direction)))))) . head)

    ((lambda (slur dir)
       ;; if attached-to-stem
       (and (attached-to-stem slur dir)
	    ;; and got beam
	    ;; urg, code dup
	    (let* ((note-columns (get-pointer slur 'note-columns))
		   (col (if (= dir 1) (car note-columns) (car (reverse note-columns))))
		   (stem (get-pointer col 'stem)))
	    (and stem
		 (get-pointer stem 'beam)
		 ;; and beam on same side as slur
		 (let ((beaming (get-property stem 'beaming)))
		   (if (pair? beaming)
		       (>= 1
			   (if (= dir -1) (car beaming) (cdr beaming)))
		       #f)))))) . stem)

    ((lambda (slur dir) (not (attached-to-stem slur dir))) . loose-end)

    ;; default case, attach to head
    ((lambda (x y) #t) . head)
    ))


(define slur-extremity-offset-alist
  '(
    ((head 1 1) . (-0.25 . 0.2))
    ((head 1 -1) . (-0.25 . -0.25))
    ((head -1 1) . (-0.25 . 0.25))
    ((head -1 -1) . (-0.85 . -0.2))

    ((stem 1 1) . (0 . 0.5))
    ((stem -1 -1) . (0 . -0.5))
    ))
