
\version "2.3.17"

\header{
texidoc="
Beams should look the same.
"
}
    \paper { raggedright= ##t }

    
\score { 
  \context Voice \relative c {
			 d''8[ d d]  d[ g d]
			c c
  }
}
