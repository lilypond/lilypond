\version "2.25.22"

\header {
  categories = "Rhythms, Tweaks and overrides"

  texidoc = "
Alternative shapes for flags on eighth and shorter notes can be displayed by
overriding the @code{stencil} property of @code{Flag}.  LilyPond provides
the following functions: @code{modern-straight-flag},
@code{old-straight-flag}, and @code{flat-flag}.  Use @code{\\revert} to
restore the default shape.

To get stacked (i.e., vertically more compact) flags, call the command
@code{\\flagStyleStacked}, which can be reset with
@code{\\flagStyleDefault}.

Overriding the @code{Flag} stencil does not change how flag elements are
positioned vertically.  This is especially noticeable for flat flags:
LilyPond doesn't dynamically adjust the vertical gaps between flag elements
in the same way as it does for beams.  A possible solution to harmonize the
appearance is to replace flat flags with half beams, as shown in the second
staff; however, this can't be done automatically.  In the code of this
snippet, such half beams are entered with @code{@@} as a prefix, for example
@code{@@c8}.

Be aware that half beams are @emph{not} @code{Flag} grobs.  This means in
particular that modifying @code{Flag} properties won't have any effect on
them (you have to use @code{Beam} properties instead), and properties for
their associated @code{Stem} grob will also behave beam-like.
"

  doctitle = "Using alternative flag styles"
}


"@" =
#(define-music-function (music) (ly:music?)
  #{ \set stemLeftBeamCount = 0 $music [] #})

testnotes = {
  \autoBeamOff
  c8 d16 e''32 f64 \acciaccatura { g,,,8 } a128 b
}

\relative c' {
  \override TextScript.staff-padding = 6
  \time 1/4
    <>^"default" \testnotes
  \override Flag.stencil = #modern-straight-flag
    <>_"modern straight" \testnotes
  \override Flag.stencil = #old-straight-flag
    <>^"old straight" \testnotes
  \override Flag.stencil = #flat-flag
    <>_"flat" \testnotes
  \revert Flag.stencil

  \flagStyleStacked
    <>^"stacked" \testnotes
  \flagStyleDefault
    <>_"default" \testnotes
}

\relative c' {
  \time 3/4
  \override Flag.stencil = #flat-flag

  <>^"flat" c8 c[ c] d16 d[ d] e''32 e[ e] f64 f[ f]
    \acciaccatura { g,,,8 } a128 a[ a a a a]
  <>^"beam-like" @c8 c[ c] @d16 d[ d] @e''32 e[ e] @f64 f[ f]
    \acciaccatura { g,,,8 } @a128 a[ a a a a]
}

\layout {
  indent = 0
  \context {
    \Score
    \override NonMusicalPaperColumn.line-break-permission = ##f
  }
}
