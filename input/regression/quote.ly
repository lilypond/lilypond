\header
{

    texidoc = "With @code{\quote}, fragments of previously entered
music may be quoted. " 

}
\version "2.1.25"

bla = \notes\relative c' { fis4 fis fis fis }

#(add-quotable "bla" bla)

\score {
    \notes \relative c'' {
	c8 d8 \quote 2 "bla" es8 gis  
	}
}
    
