#(ly:set-option 'old-relative)
\version "1.9.1"
\header {
texidoc = "Explicit beams may cross barlines. "
}
\score {
\notes \relative c'' { c2.  c8[ c8 c8 c8] }

    \paper { raggedright= ##t }

}
