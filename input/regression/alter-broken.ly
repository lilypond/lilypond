\version "2.23.14"

\header {
  texidoc = "The command @code{\\alterBroken} may be used to override the
pieces of a broken spanner independently.  The following example demonstrates
its usage with a variety of data types."
}

\layout {
  ragged-right = ##t
}

#(ly:set-option 'warning-as-error)
#(ly:expect-warning "NoteHead.color: this grob is not a spanner")
#(for-each
   (lambda _ (ly:expect-warning "TimeSignature.color: this grob is not a spanner"))
   (iota 2))
#(ly:expect-warning "not a spanner")

\relative {
  d''4-\alterBroken #'positions #'((3 . 3) (5 . 5))
    -\alterBroken #'color #'((0 0 1) (1 0 0))
    -\alterBroken #'dash-definition #'( ((0 1 0.4 0.75))
                                        ((0 0.5 0.4 0.75) (0.5 1 1 1)) )
    -(
  d' b g
  \break
  d d' b g)
  \alterBroken #'padding #'(1 3) Staff.OttavaBracket
  % It is possible to use procedures as arguments.
  c-\alterBroken Hairpin.stencil #`(
      ,ly:hairpin::print
      ,(lambda (grob)
        (ly:stencil-rotate (ly:hairpin::print grob) -10 0 0)))
    \<
  d e f
  \break
  c
  f,
  % It is also possible to use unpure/pure containers.  A noteworthy use
  % case involves the unpure/pure containers returned by grob-transformer.
  c\!-\alterBroken Hairpin.stencil #`(
       ,(grob-transformer 'stencil
         (lambda (grob orig)
           orig))
       ,(grob-transformer 'stencil
         (lambda (grob orig)
           (ly:stencil-rotate orig -5 0 0))))
    \>
  % Since `NoteHead' is not the name of a spanner, the following has no
  % effect on layout.  Warnings (suppressed here) are issued.
  \once \alterBroken #'color #`(,red ,blue) NoteHead
  \once\alterBroken #'color #`(() ,blue) Tie
  \once\alterBroken #'control-points #'(
     ((1 . 3) (2 . 4) (3 . 4) (4 . 3))
     ((3 . 3) (4 . 4) (5 . 4) (6 . 3))
    ) Tie
  f'~
  % Also a warning.
  \once \alterBroken #'color #`(,red ,blue) Staff.TimeSignature
  \time 2/4
  \break
  % Another warning
  \alterBroken stencil #'(#f) f
  c a f\!
}
