\version "1.7.18"

\header{
texidoc="
Beams should look the same.
"
}
    \paper { raggedright= ##t }

    
\score { 
  \context Voice \notes\relative c {
			 d''8-[ d d]  d-[ g d]
			c c
  }
}
