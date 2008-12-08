\version "2.11.64"

\header {
  lsrtags = "expressive-marks, text"

  texidoc = "
This example provides a function to typeset a hairpin (de)crescendo
with some additional text below it, such as \"molto\" or \"poco\". The
example also illustrates how to modify the way an object is normally
printed, using some Scheme code.
"
  doctitle = "Center text below hairpin dynamics"
}

hairpinWithCenteredText =
#(define-music-function (parser location text) (markup?)
#{
  \override Voice.Hairpin #'stencil = #(lambda (grob)
    (ly:stencil-aligned-to
     (ly:stencil-combine-at-edge
      (ly:stencil-aligned-to (ly:hairpin::print grob) X CENTER)
      Y DOWN
      (ly:stencil-aligned-to (grob-interpret-markup grob $text) X CENTER))
     X LEFT))
#})

hairpinMolto = \hairpinWithCenteredText \markup { \italic molto }
hairpinMore = \hairpinWithCenteredText \markup { \larger moltissimo }

\layout { ragged-right = ##f }

{
  \hairpinMolto c'2\< c'\f
  \hairpinMore  c'2\< c'\f
}
