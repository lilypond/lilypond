\version "1.7.3"

#(define (make-text-checker text)
   (lambda (grob) (equal? text (ly:get-grob-property grob 'text))))

\score {
  \notes\relative c''' {
    \property Voice.Stem \set #'direction = #1
    \outputproperty #(make-text-checker "m.d.")
      #'extra-offset = #'(-3.5 . -4.5)
    a^2^"m.d."
  }
  \paper { linewidth = -1. }
}
