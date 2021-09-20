\version "2.23.4"

\header {
  texidoc = "The functions @code{ly:engraver-make-item} and
@code{ly:engraver-make-spanner} are similar to
@code{ly:engraver-make-grob}.  They are useful when the grob
definition does not mandate a particular grob class."
}

#(define (prepare-balloon balloon parent class sgn)
   (ly:grob-set-object! balloon 'sticky-host parent)
   (ly:grob-set-parent! balloon X parent)
   (ly:grob-set-parent! balloon Y parent)
   (ly:grob-set-property!
     balloon
     'text
     (format #f "~a ~a" class (grob::name parent)))
   (ly:grob-set-property! balloon 'X-offset (* sgn 3))
   (ly:grob-set-property! balloon 'Y-offset (* sgn 5))
   (ly:grob-set-property! balloon 'annotation-balloon #f)
   (ly:grob-set-property! balloon 'font-size -5))

% A balloon on many grobs.  This would be simpler with ly:engraver-make-sticky,
% but the goal is to test ly:engraver-make-item and ly:engraver-make-spanner.

\new Voice \with {
  \consists
    #(lambda (context)
       (let ((items '())
             (spanners '()))
         (make-engraver
           (acknowledgers
             ((item-interface engraver grob source-engraver)
                (set! items (cons grob items)))
             ((spanner-interface engraver grob source-engraver)
                (set! spanners (cons grob spanners))))
           ((process-acknowledged engraver)
              (for-each
                (lambda (item)
                  (if (grob::has-interface item 'note-head-interface) ;; issue #6155
                      (let ((balloon
                              (ly:engraver-make-item engraver 'BalloonText item)))
                        (prepare-balloon balloon item 'Item 1))))
                items)
              (set! items '())
              (for-each
                (lambda (spanner)
                  (let ((balloon
                          (ly:engraver-make-spanner engraver 'BalloonText spanner)))
                    (prepare-balloon balloon spanner 'Spanner -1)))
                spanners)
              (set! spanners '())))))
}
{ c'\< d'8 d' e2\! }
