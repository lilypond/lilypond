\header {

  texidoc = "The order of setting nested properties does not influence
  text spanner layout."

}

\version "2.19.21"

sample = \relative {
  c''2\startTextSpan c2 \break
  c2 c2 \stopTextSpan
}

<< {
  \override TextSpanner.bound-details.left-broken.text =
    \markup { \large "BROKEN" }
  \override TextSpanner.bound-details.left.text =
    \markup { "text" }
  \sample
} \\ {
  \override TextSpanner.bound-details.left.text =
    \markup { "text" }
  \override TextSpanner.bound-details.left-broken.text =
    \markup { \large "BROKEN" }
  \sample
} >>
