
\version "2.19.21"
\header {

  texidoc = "Upstem notes before a bar line are printed with some extra
space.  This is an optical correction similar to juxtaposed stems.
"

}

\layout { ragged-right = ##t}


\relative
{
  \override Score.PaperColumn.layer = #1
  \override Score.PaperColumn.stencil = #ly:paper-column::print

  
  \time 2/8
  \stemUp
  e'8[ e]
  e'[ e]
}



