#(ly:set-option 'old-relative)
\header { texidoc = "

Setting @code{\outputproperty} is a clumsy and deprecated way of changing
grob definitions.  In this example, the predicate checks the @code{text}
object property, to shift only the `m.d.'  text, but not the `two'
text.  Since both scripts are @code{TextScript} grobs, using
possible to use @code{\override} would shift both texts.

"

}

\version "1.9.1"

#(define (make-text-checker text)
   (lambda (grob) (equal? text (ly:get-grob-property grob 'text))))

\score {    
  \notes\relative c''' {
    a^"two"^"m.d."
    \context Voice
      \outputproperty #(make-text-checker (make-simple-markup "m.d."))
        #'extra-offset = #'(-4.0 . -3.5)
    a^"two"^"m.d."    
    a^"two"^"m.d."    
  }
}
