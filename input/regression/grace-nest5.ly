#(ly:set-option 'old-relative)
\version "1.9.0"
\header {
    texidoc = "Another nested grace situation."
    }

\score
{ \notes \relative c'' {
     s2 <<f>>4
    \grace g e4
}

  \paper { raggedright = ##t }
}

