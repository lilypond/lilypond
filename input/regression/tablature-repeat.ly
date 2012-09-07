\header
{
  texidoc = "How a repeat sign looks in tablature."
}

\version "2.16.0"
\paper
{
  ragged-right = ##t
}

\new TabStaff
{
  \repeat volta 2 s1
}
