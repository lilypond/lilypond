\version "2.23.0"

\header {
  texidoc = "Per default notes played on open strings are disregarded by
@code{BendSpanner} unless the property @code{'bend-me} is set to true for this
note.  Other notes may be excluded by setting the property @code{'bend-me} to
false."
}

bend-excludes = {
  <>^\markup { \circle "3" is open/bend }
  <>^"open strings are not bent unless 'bend-me is set true."
  <g d' f'>4 \^
  <g dis' fis'> \^
  <g d' f'>2

  <\tweak bend-me ##t g d' f'>4 \^
  <\tweak bend-me ##t aes dis' fis'> \^
  <g d' f'>2

  \bar "."
  <>^\markup { \circle "2" is open/bend }

  <a b f'>4\^
  <ais b fis'>\^
  <a b f'>2

  <a \tweak bend-me ##t b f'>4\^
  <ais \tweak bend-me ##t bis fis'>\^
  <a b f'>2

  \bar "."
  <>^\markup { \circle "1" is open/bend }

  <a c' e'>4\^
  <ais cis' e'>\^
  <a c' e'>2

  <a c' \tweak bend-me ##t e'>4\^
  <ais cis' eis'>\^
  <a c' e'>2

  \bar "."
  \break

  <>^"other notes excluded via \\tweak bend-me ##f"

  %% bend up
  <g\4 \tweak bend-me ##f b\3 d'\2 >4\^
  <a\4 e'\2 >

  %% bend down
  <a\4 cis'\3 e'\2 >4\^
  <g\4 \tweak bend-me ##f b\3  d'\2 >

  %% bend up and down
  <g\4 \tweak bend-me ##f b\3 d'\2 >4\^
  <a\4 e'\2 >\^
  <g\4 \tweak bend-me ##f b\3 d'\2 >

  \bar "|."
}

\score {
  \new StaffGroup
  <<
    \new Staff { \clef "G_8" \bend-excludes }
    \new TabVoice \bend-excludes
  >>
  \layout {
    \context {
      \Voice
      \omit StringNumber
    }
  }
}
