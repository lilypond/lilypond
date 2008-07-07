\header{

  texidoc = "Skyline horizontal spacing may fold non-adjacent columns
together, but they still do not collide. In this case, the arpeggio
and the barline do not collide."

}

\version "2.11.51"

\paper
{
  ragged-right = ##t
}

\new Staff
\relative c
{
  \override Score.NonMusicalPaperColumn #'stencil = #ly:paper-column::print 
  \time 6/8
  \clef bass
  s2. |
  \relative c <<
    {
      <des ges b des>4\arpeggio
    }
    \\
    {
      \acciaccatura ges,8 \voiceTwo ges4
    }
  >>
}
