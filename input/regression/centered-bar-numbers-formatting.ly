\version "2.23.9"

\header {
  texidoc = "Centered bar numbers may be boxed or circled.  Their
appearance can be changed through properties of the
@code{text-interface}."
}

\layout {
  \context {
    \Score
    centerBarNumbers = ##t
    \override CenteredBarNumber.stencil
      = #(make-stencil-boxer 0.1 0.25 ly:text-interface::print)
    \override CenteredBarNumber.font-shape = #'italic
    \override CenteredBarNumber.font-series = #'bold
  }
}

{
  \repeat unfold 15 { c'1 }
  \break
  \override Score.CenteredBarNumber.stencil
    = #(make-stencil-circler 0.1 0.25 ly:text-interface::print)
  \repeat unfold 15 { c'1 }
}