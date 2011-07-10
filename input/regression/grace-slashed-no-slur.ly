\version "2.15.6"

\header
{
  texidoc = "Create grace notes with slashed stem, but no slur. That can be used
  when the grace note is tied to the next note.
"
}
\relative {
  \slashedGrace c16~ c1
}
