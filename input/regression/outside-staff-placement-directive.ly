\version "2.17.0"

\header {
  texidoc = "@code{VerticalAxisGroup} grobs can place outside staff objects
using one of the four directives shown below.
"
}


\layout {
  ragged-right = ##t
  indent = 0.0
  \context {
    \Voice
    \remove "Ligature_bracket_engraver"
    \consists "Mensural_ligature_engraver"
  }
  \context {
    \Score
    \override SpacingSpanner #'packed-spacing = ##t
    \override PaperColumn #'keep-inside-line = ##f
  }
}

music = \context Voice {
  \clef "petrucci-c4"
  \set Staff.printKeyCancellation = ##f
  \cadenzaOn % turn off bar lines
  \accidentalStyle "forget"
  \textLengthOn

% ligaturae binaria

  \[
    b\breve^\markup { \column { { \bold "ligaturae binaria" } "BL" } }
    g\longa
    \]

  \[
    g\breve^\markup { "BL" }
    b\longa
    \]

  \[
    b\longa^\markup { "LL" }
    g
    \]

  \[
    g\longa^\markup { "LL" }
    b
    \]

  \[
    b\breve^\markup { "BB" }
    g
    \]

  \[
    g\breve^\markup { "BB" }
    b
    \]

  \[
    b\longa^\markup { "LB" }
    g\breve
    \]

  \[
    g\longa^\markup { "LB" }
    b\breve
    \]

  \[
    b1^\markup { "SS" }
    g
    \]

  \[
    g1^\markup { "SS" }
    b
    \]

  \bar "|"
}

{
  \override Staff.VerticalAxisGroup #'outside-staff-placement-directive =
    #'left-to-right-polite
  \music
}
{
  \override Staff.VerticalAxisGroup #'outside-staff-placement-directive =
    #'left-to-right-greedy
  \music
}
{
  \override Staff.VerticalAxisGroup #'outside-staff-placement-directive =
    #'right-to-left-polite
  \music
}
{
  \override Staff.VerticalAxisGroup #'outside-staff-placement-directive =
    #'right-to-left-greedy
  \music
}
