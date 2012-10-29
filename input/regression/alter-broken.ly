\version "2.17.6"

\header {
  texidoc = "The command @code{\\alterBroken} may be used to override the
pieces of a broken spanner independently.  The following example demonstrates
its usage with a variety of data types."
}

\layout {
  ragged-right = ##t
}

#(ly:expect-warning (_ "not a spanner name"))

\relative c'' {
  \alterBroken #'positions #'((3 . 3) (5 . 5)) Slur
  \alterBroken #'color #'((0 0 1) (1 0 0)) Slur
  \alterBroken #'dash-definition #'( ((0 1 0.4 0.75))
                                          ((0 0.5 0.4 0.75) (0.5 1 1 1)) ) Slur
  d4( d' b g
  \break
  d d' b g)
  \alterBroken #'padding #'(1 3) Staff.OttavaBracket
  % Spaces in spanner's name are disregarded.
  \alterBroken #'style #'(line dashed-line) Staff.OttavaBracket
  \ottava #1
  % It is possible to use procedures as arguments.
  \alterBroken #'stencil #`(
    ,ly:hairpin::print
    ,(lambda (grob)
      (ly:stencil-rotate (ly:hairpin::print grob) -5 0 0))) Hairpin
  c\< d e
  % Since `NoteHead' is not the name of a spanner, the following has no
  % effect on layout.  A warning (suppressed here) is issued.
  \alterBroken #'color #`(,red ,blue) NoteHead
  \alterBroken #'color #`(() ,blue) Tie
  \alterBroken #'control-points #'(
     ((1 . 3) (2 . 4) (3 . 4) (4 . 3))
     ((3 . 3) (4 . 4) (5 . 4) (6 . 3))
    ) Tie
  f~
  \break
  f c a f\!
  \ottava #0
}
