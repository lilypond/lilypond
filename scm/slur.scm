;;;;
;;;; slur.scm -- Slur scheme stuff
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2000--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;

(define (attached-to-stem slur dir)
  (let* ((note-columns (ly:grob-property slur 'note-columns))
	 (col (if (= dir 1) (car note-columns) (car (last-pair note-columns))))
	 (stem (ly:grob-property col 'stem)))
    (and
     (eq? col (ly:spanner-get-bound slur dir))
     (ly:grob? stem)
     (ly:grob-property stem 'heads))))


;;
;; Currently, we have attachments:
;;
;;    'head 'along-side-stem 'stem 'loose-end
;;
(define (calc-slur-extremity slur dir)
  (let* ((note-columns (ly:grob-property slur 'note-columns))
	 (col (car (if (= dir 1) note-columns (reverse note-columns))))
	 (stem (ly:grob-property col 'stem)))


   (cond
    ((< (length note-columns) 1) 'head)
    ((not (attached-to-stem slur dir)) 'loose-end)
    ((and stem
	  (not (equal? (ly:grob-property slur 'direction) 
		       (ly:grob-property stem 'direction))))  'head)
    ((and (attached-to-stem slur dir)
	  (ly:grob? stem)
	  (ly:grob? (ly:grob-property stem 'beam))
	  ;; and beam on same side as slur
	  (equal?
	   (ly:grob-property stem 'direction)
	   (ly:grob-property slur 'direction)))
     'stem)
    ((not (attached-to-stem slur dir))  'loose-end)
    (else 'head))
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
    ((head 1 1) . (-0.25 . 0.75))
    ((head 1 -1) . (-0.25 . 0.75))
    ((head -1 1) . (-0.25 . 0.75))
    ((head -1 -1) . (-0.85 . 0.75))

    ((stem 1 1) . (-0.125 . 0.5))
    ((stem -1 -1) . (-0.125 . 0.5))

    ((loose-end 1 1) . (-0.4 . 0))
    ((loose-end 1 -1) . (-0.4 . 0))
    ((loose-end -1 -1) . (-4 . 0))
    ((loose-end -1 1) . (-4 . 0))
    ))

;; This is a bit of a hack: slurs and phrasing slurs
;; attaching at the same note must not collide.
;; However, slurs (and phrasing slurs) should look
;; at scripts and eachother.
(define default-phrasing-slur-extremity-offset-alist
  '(
    ((head 1 1) . (-0.25 . 1.25))
    ((head 1 -1) . (-0.25 . 1.25))
    ((head -1 1) . (-0.25 . 1.25))
    ((head -1 -1) . (-0.85 . 1.25))

    ((stem 1 1) . (-0.25 . 1.5))
    ((stem -1 -1) . (-0.25 . 1.5))

    ((loose-end 1 1) . (-0.4 . 0))
    ((loose-end 1 -1) . (-0.4 . 0))
    ((loose-end -1 -1) . (-4 . 0))
    ((loose-end -1 1) . (-4 . 0))
    ))


