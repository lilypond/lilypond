
\version "2.1.22"
\header {
    texidoc = "Another nested grace situation."
    }

\score
{ \notes \relative c'' {
     s2 <f>4
    \grace g e4
}

  \paper { raggedright = ##t }
}

