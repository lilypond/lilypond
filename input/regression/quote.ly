\header
{

    texidoc = "With @code{\quote}, fragments of previously entered
music may be quoted. " 

}
\version "2.3.4"

\addquote bla \relative c' {
    fis4 g a b }

\score {
     \relative c'' {
	c8 d8 \quote bla 2 es8 gis  
    }
}

