
\version "2.3.4"
\header {
texidoc = "Explicit beams may cross barlines. "
}
\score {
 \relative c'' { c2.  c8[ c8 c8 c8] }

    \paper { raggedright= ##t }

}
