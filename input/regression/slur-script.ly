
\header {

  texidoc = "A slur avoids collisions with scripts, which are placed
    either inside or outside the slur, depending on the script.  The
    slur responds appropriately if a script is moved."
}

\version "2.19.21"
\layout {
  indent = 0\mm
  ragged-right = ##t
}
\relative
{
  b4-.( b-.)
  \once \override Script.padding = #1.5
  b-.( b-.)
  b_\downbow( b_\downbow)
  \once \override Script.padding = #1.5
  b_\downbow( b_\downbow)
}
