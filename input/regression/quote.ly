\header
{

    texidoc = "With @code{\quote}, fragments of previously entered
music may be quoted. " 

}
\version "2.1.26"

\addquote bla \notes\relative c' { fis4 g a b }

\score {
    \notes \relative c'' {
	c8 d8 \quote 2 bla es8 gis  
    }
}

