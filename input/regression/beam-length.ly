
\version "2.3.22"

\header{
texidoc="
Beams should look the same.
"
}
    \layout { raggedright= ##t }

    
\score { 
  \context Voice \relative c {
			 d''8[ d d]  d[ g d]
			c c
  }
}
