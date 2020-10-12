\version "2.23.0"

\header {
  texidoc = "A @code{BendSpanner} prints a line and/or curve to a certain point
above the @code{TabStaff} or above the target @code{TabNoteHead}.  This line or
curve ends in an arrow head.  For an up-pointing @code{BendSpanner} the amount
of bending is printed above the arrow head.  For a down-pointing
@code{BendSpanner} the target @code{TabNoteHead} will be parenthesized.
Works at line breaks."
}

bend-up-and-down = {
  <>^"simple bends up and down"
  g,4\^ ais, ais,\^ g,
  c4\^ dih\^ c2
  f4\^ g\^ f2
  bes4\^ bih \^ bes2
  d'4\^ e'\^ d'2
  g'4\^ a'\^ g'2
  \break
  <>^"double bends up and down"
  <g, c>4\^ <a, d> <a, d>\^ <g, c>
  <g, f>4\^ <ais, gis>\^ <g, f>2
  <g, bes>4\^ <a, c'>\^ <g, bes>2
  <g, d'>4\^ <ais, eis'>\^ <g, d'>2
  <g, g'>4\^ <ais, ais'>\^ <g, g'>2
  <d' fis'>4\^ <eis' gisis'>\^ <d' fis'>2
  <d' fis'>4\^ <eis' gisis'>\^ <fis' d'>2
  <>^"bends up and down"
  \break
  g,2\^ ais,\^ \break  g,1
  g,1\^ \break a,\^ g,1
  <d' fis'>2\^ <eis' gisis'>\^
  \break <d' fis'>1
  <d' fis'>1\^
  \break <eis' gisis'>\^ <d' fis'>1
  \bar "|."
}

\score {
  \new StaffGroup
  <<
    \new Staff { \clef "G_8" \bend-up-and-down }
    \new TabVoice \bend-up-and-down
  >>
  \layout {
    ragged-right = ##t
    \context {
      \Voice
      \omit StringNumber
    }
    \context {
      \TabStaff
      minimumFret = #3
      restrainOpenStrings = ##t
    }
  }
}
