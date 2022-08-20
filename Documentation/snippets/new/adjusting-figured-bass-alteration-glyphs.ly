\version "2.23.11"

\header {
  lsrtags = "chords"

  texidoc = "
In figured bass, specially designed glyphs for @code{6\\\\},
@code{7\\\\}, and @code{9\\\\} are used by default.  Similarly,
specially designed glyphs for symbols @code{2\\+}, @code{4\\+}, and
@code{5\\+} are used by default if plus signs appear after the number.

To change that, pass an alist to @code{figuredBassPlusStrokedAlist} and
set the glyph in question to @code{#f} (or omit it).
"

  doctitle = "Adjusting figured bass alteration glyphs"
}


\figures {
  \set figuredBassPlusDirection = #RIGHT
  <6\\> <7\\> <9\\> r
  <2\+> <4\+> <5\+> r

  \set figuredBassPlusStrokedAlist =
    #'((2 . "figbass.twoplus")
       ;; (4 . "figbass.fourplus")
       ;; (5 . "figbass.fiveplus")
       (6 . "figbass.sixstroked")
       ;; (7 . "figbass.sevenstroked")
       ;; (9 . "figbass.ninestroked")
      )
  <6\\> <7\\> <9\\> r
  <2\+> <4\+> <5\+> r
}
