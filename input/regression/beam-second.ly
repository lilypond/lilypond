
\version "2.16.0"

\header{

  texidoc="Engraving second intervals is tricky.  We used to have
problems with seconds being too steep, or getting too long stems.  In
a file like this, showing seconds, you'll spot something fishy very
quickly."

}

\layout{
  ragged-right = ##t
}

\relative c''{
  \stemUp
  b8[ c]
  b16[ c]
  a'[ b]
}
