\version "2.23.14"

\header{

  texidoc=" Clefs with @code{full-size-change} should be typeset in
full size."

}

\layout{
  ragged-right = ##t
}

clefs = {
  \clef "treble"        c'1^"treble"        \bar "||"
  \clef "french"        c'1^"french"        \bar "||"
  \clef "soprano"       c'1^"soprano"       \bar "||"
  \clef "mezzosoprano"  c'1^"mezzosoprano"  \bar "||"
  \clef "alto"          c'1^"alto"          \bar "||"
  \clef "varC"          c'1^"varC"          \bar "||"
  \clef "treble"        c'1^"treble"        \bar "||"
  \clef "altovarC"      c'1^"altovarC"      \bar "||"
  \clef "tenor"         c'1^"tenor"         \bar "||"
  \clef "tenorvarC"     c'1^"tenorvarC"     \bar "||"
  \clef "tenorG"        c'^"tenorG"         \bar "||"
  \clef "GG"            c'1^"GG"            \bar "||"
  \clef "baritone"      c'1^"baritone"      \bar "||"
  \clef "varbaritone"   c'1^"varbaritone"   \bar "||"
  \clef "baritonevarC"  c'1^"baritonevarC"  \bar "||"
  \clef "baritonevarF"  c'1^"baritonevarF"  \bar "||"
  \clef "bass"          c'1^"bass"          \bar "||"
  \clef "subbass"       c'1^"subbass"       \bar "||"
  \clef "percussion"    c'1^"percussion"    \bar "||"
  \clef "varpercussion" c'1^"varpercussion" \bar "||"
}

{ 
  \textLengthOn
  \textMark "clefs:"
  \clefs
  \override Staff.Clef.full-size-change = ##t \break
  \textMark "with full-size-change = #t:"
  \clefs
  \bar "|."
}

