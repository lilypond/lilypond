\version "2.13.4"

\header {
  texidoc = "Bound padding for hairpins also works with neighboring
@code{DynamicTextSpanner} grobs.  In this case, @code{bound-padding}
is not scaled down.
"
}

\relative c' {
  \override Hairpin #'to-barline = ##f
  c2\>
  \dimTextDim
  c2\>
  \dimHairpin
  c\> c\! \break
  \dimTextDim
  c2\> 
  \override Hairpin #'bound-padding = #5
  \dimHairpin
  c2\>
  \dimTextDim
  c2\> c\! \break
  \crescHairpin
  c2\< c\<
  c2\< c\!
}
