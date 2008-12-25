\version "2.12.0"

\header{

  texidoc=" Clefs with @code{full-size-change} should be typeset in
full size."

}

\layout{
  ragged-right = ##t
}


{ 
  \textLengthOn
  \clef "treble" c'1^"treble" \bar "||"
  \clef "french"c'1^"french" \bar "||"
  \clef "soprano"c'1^"soprano" \bar "||"
  \clef "mezzosoprano"c'1^"mezzosoprano" \bar "||"
  \clef "alto"c'1^"alto" \bar "||"
  \clef "tenor"c'1^"tenor" \bar "||"
  \clef "baritone"c'1^"baritone" \bar "||"
  \clef "varbaritone"c'1^"varbaritone" \bar "||"
  \clef "bass"c'1^"bass" \bar "||"
  \clef "subbass"c'1^"subbass" \bar "||"
  \override Staff.Clef  #'full-size-change = ##t 
  \clef "treble" c'1^"full-size-change = #t" \bar "|."
}

