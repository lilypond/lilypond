\version "2.25.35"

\header {
  categories = "Editorial annotations, Fretted strings, Text, Tweaks
                and overrides"

  texidoc = "
By default, markups are not displayed in a tablature.

To make them appear, revert the @code{stencil} property of the
@code{TextScript} grob in the @code{TabStaff} context.
"

  doctitle = "Adding markups in a tablature"
} % begin verbatim



high  = { r4 r8 <g c'> q r8 r4 }
low = { c4 r4 c8 r8 g,8 b, }
pulse = { s8^"1" s^"&" s^"2" s^"&" s^"3" s^"&" s^"4" s^"&" }

\score {
 \new TabStaff {
   \*2 << \high \\ \low \\ \pulse >>
  }
  \layout {
    \context {
      \TabStaff
      \clef moderntab
      \revert TextScript.stencil
      \override TextScript.font-series = #'bold
      \override TextScript.font-size = #-2
      \override TextScript.color = #red
    }
    \context {
      \Score
      proportionalNotationDuration = #1/8
    }
  }
}
