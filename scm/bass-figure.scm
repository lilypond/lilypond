;;;; bass-figure.scm -- implement Scheme output routines for TeX
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2005 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@cs.uu.nl>


(ly:add-interface
 'bass-figure-interface
 "A bass figure, including bracket"
 '())


(define-public (format-new-bass-figure figure event context)
  (let* ((fig (ly:music-property event 'figure))
	 (fig-markup (if (number? figure)
			 (markup #:number (number->string figure 10))
			 #f
			 ))

	 (alt (ly:music-property event 'alteration))
	 (alt-markup
	  (if (number? alt)
	      (markup
		      #:general-align Y DOWN #:smaller #:smaller
		      (alteration->text-accidental-markup alt))
	      
	      #f))
	 (alt-dir (ly:context-property context 'figuredBassAlterationDirection))
	 )

    (if (and (not fig-markup) alt-markup)
	(begin
	  (set! fig-markup (markup #:left-align #:pad-around 0.3 alt-markup))
	  (set! alt-markup #f)))


    ;; hmm, how to get figures centered between note, and
    ;; lone accidentals too?
    
    ;;    (if (markup? fig-markup)
    ;;	(set!
    ;;	 fig-markup (markup #:translate (cons 1.0 0)
    ;;			    #:hcenter fig-markup)))

    (if alt-markup
	(set! fig-markup
	      (markup #:put-adjacent
		      fig-markup X
		      (if (number? alt-dir)
			  alt-dir
			  LEFT)
		      #:pad-around 0.2 alt-markup
		      )))

    (if (markup?  fig-markup)
	fig-markup
	empty-markup)))

(define-public (format-bass-figure figures context grob)
  ;; TODO: support slashed numerals here.
  (define (fig-to-markup fig-music)
    (let* ((align-accs
	    (eq? #t (ly:context-property context 'alignBassFigureAccidentals)))
	   (fig (ly:music-property fig-music 'figure))
	   (acc (ly:music-property fig-music 'alteration))
	   (acc-markup #f)
	   (fig-markup
	    (if (markup? fig)
		fig
		(if align-accs (make-simple-markup " ")
		    (if (not (eq? acc '()))
			(make-simple-markup "")
			(make-strut-markup))))))

      (if (number? acc)
	  (make-line-markup (list fig-markup
				  (alteration->text-accidental-markup acc)))
	  fig-markup)))

  (define (filter-brackets i figs acc)
    (cond
     ((null? figs) acc)
     (else
      (filter-brackets (1+ i) (cdr figs)

		       (append
			(if (eq? (ly:music-property (car figs) 'bracket-start) #t)
			     (list i)
			     '())
			(if (eq? (ly:music-property (car figs) 'bracket-stop) #t)
			     (list i)
			     '())
			
			acc)))))

  (set! (ly:grob-property grob 'text)
	(make-bracketed-y-column-markup
	 (sort (filter-brackets 0 figures '()) <)
	 (map fig-to-markup figures))))
