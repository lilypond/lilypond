\version "1.5.68"
\header {

texidoc = "overriding the molecule callback can also be used to draw a
 box around arbitrary grobs.

 TODO: check whether the corners are really correct.

 TODO:  circled molecules.
  
 "
}

#(define (make-molecule-boxer callback)
  (define (molecule-boxer grob)
  (let*
   (
    (mol    (callback grob))
    (box-padding 0.1)
    (x-ext (widen-interval (ly-get-molecule-extent mol 0) box-padding))
    (y-ext (widen-interval (ly-get-molecule-extent mol 1) box-padding))
    (rule-thick 0.1)
    (x-rule (box-molecule (widen-interval x-ext rule-thick)
                              (cons 0 rule-thick)))
    (y-rule (box-molecule (cons 0 rule-thick) y-ext))
    )
    
    (set! mol (ly-combine-molecule-at-edge mol 0 1 y-rule (* 0.5 box-padding)))
    (set! mol (ly-combine-molecule-at-edge mol 0 -1  y-rule (* 0.5 box-padding)))
    (set! mol (ly-combine-molecule-at-edge mol 1 1  x-rule 0.0))  
    (set! mol (ly-combine-molecule-at-edge mol 1 -1 x-rule 0.0))
    
    mol
 ))
 molecule-boxer
 )


 \score { \notes \relative c''  {

 \property Voice.TextScript \override #'molecule-callback =
   #(make-molecule-boxer Text_item::brew_molecule)

   c'4^"foo"

\property Voice.Stem \override #'molecule-callback =
   #(make-molecule-boxer Stem::brew_molecule)

\property Score.RehearsalMark \override #'molecule-callback =
   #(make-molecule-boxer Text_item::brew_molecule)

      c8
\property Voice.Stem \revert #'molecule-callback

      c4. c4 \mark "F" c1 
   }}
