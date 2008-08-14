\header {

    texidoc = "When notes are colliding, the resolution depends on the
    dots: notes with dots should go to the right, if there could be
    confusion to which notes the dots belong."
}
\layout {  ragged-right = ##t }

\version "2.11.51"
\relative c'' { 
  << <a c>2\\ { <b d>4 <b d>4 }   >>
  << { <a c>2 } \\ { <b d>4. <b e>8 } >> 
}
 
