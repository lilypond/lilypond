
\version "2.12.0"
\header {

  texidoc = "Upstem notes before a barline are printed with some extra
space. This is an optical correction similar to juxtaposed stems.
"

}

\layout { ragged-right = ##t}


\relative e'
{
  \override Score.PaperColumn #'layer = #1
  \override Score.PaperColumn #'stencil = #ly:paper-column::print

  
  \time 2/8
  \stemUp
  e8[ e]
  e'[ e]
}



