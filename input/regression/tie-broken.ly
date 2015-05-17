\header
{
texidoc = "Ties behave properly at line breaks."
  }

\version "2.19.21"

\paper { ragged-right = ##t } 

\relative
{
  r2...  <b e a>16 ~
  \break
  <b e a>4 r4 r2
}

