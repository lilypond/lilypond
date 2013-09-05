\version "2.17.27"

\header {
  lsrtags = "contemporary-notation, expressive-marks, symbols-and-glyphs"

  texidoc = "
In order to make parts of a crescendo hairpin invisible, the following
method is used: A white rectangle is drawn on top of the respective
part of the crescendo hairpin, making it invisible.

The markup command @code{with-dimensions} tells LilyPond to consider only
the bottom edge of the rectangle when spacing it against the hairpin.
The property @code{staff-padding} prevents the rectangle from fitting
between the hairpin and staff.

Make sure to put the hairpin in a lower layer than the text markup to
draw the rectangle over the hairpin.

"
  doctitle = "Broken Crescendo Hairpin"
}

\relative c' {
  <<
    {
      \dynamicUp
      r2 r16 c'8.\pp r4
    }
    \\
    {
      \override DynamicLineSpanner.layer = #0
      des,2\mf\< ~
      \override TextScript.layer = #2
      \once\override TextScript.staff-padding = #6
      \once\override TextScript.vertical-skylines = #'()
      des16_\markup \with-dimensions #'(2 . 7) #'(0 . 0)
                    \with-color #white
                    \filled-box #'(2 . 7) #'(0 . 2) #0
      r8. des4 ~ des16->\sff
    }
  >>
}
