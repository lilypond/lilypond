
\version "2.1.22"
\header {
    texidoc = "Graces notes may have the same duration as the main note."
    }

\score
{ \notes \relative c'' {
     s2 <f>4
    \grace g e4
}

  \paper { raggedright = ##t }
}

