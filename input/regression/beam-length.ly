
\version "2.1.36"

\header{
texidoc="
Beams should look the same.
"
}
    \paper { raggedright= ##t }

    
\score { 
  \context Voice \notes\relative c {
			 d''8[ d d]  d[ g d]
			c c
  }
}
