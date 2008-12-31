\version "2.12.0"

\header {
  lsrtags = "tweaks-and-overrides"

  texidoc = "
The @code{\\applyOutput} command allows the tuning of any layout
object, in any context.  It requires a Scheme function with three
arguments."

  doctitle = "Changing properties for individual grobs"
}

#(define (mc-squared grob grob-origin context)
  (let*
    (
      (ifs (ly:grob-interfaces grob))
      (sp (ly:grob-property grob 'staff-position))
    )
    (if (memq 'note-head-interface ifs)
      (begin
        (ly:grob-set-property! grob 'stencil
          (grob-interpret-markup grob
            (make-lower-markup 0.5
              (case sp
                ((-5) "m")
                ((-3) "c ")
                ((-2) (make-smaller-markup (make-bold-markup "2")))
                (else "bla")
                ))))
        ))))

\relative c' {
  <d f g b>2
  \applyOutput #'Voice #mc-squared
  <d f g b>2
}
