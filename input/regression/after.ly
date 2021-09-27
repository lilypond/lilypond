\header
{
  texidoc = "Delayed post-events and other types of music can be
created with @code{\after} and @code{\afterGrace}."
}

\version "2.23.4"

\relative {
  \after 4 \< \after 2 \> \after 2. \! c'1
  \after 2. \tweak Stem.stencil ##f \tweak font-size -2 e4 d1
  \afterGrace 7/8 f1 g8
  <>-"one" \after 4 -"two" \after 2 -"three" \after 2. -"four" a1
  \after 1*2/3 \turn b
  <>\> \after 2..\! c
}

