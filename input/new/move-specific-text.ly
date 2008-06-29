\version "2.10.0"
% possible rename to scheme- or something like that.  -gp
\header {
  lsrtags = "tweaks-and-overrides"
  texidoc = "@cindex Scheme Move Text
Objects of the same type, like text, can be moved around by using some Scheme code.
"
  doctitle = "Move specific text"
}

#(define (make-text-checker text)
   (lambda (grob) (equal? text (ly:grob-property grob 'text))))

\score {
  \relative c''' {
    \stemUp
    \applyOutput #'Voice
    #(outputproperty-compatibility
      (make-text-checker (make-simple-markup "m.d."))
      'extra-offset '(-3.5 . -4.5))
    a^2^"m.d."
  }
  \layout { }
}
