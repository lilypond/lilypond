
\version "2.1.22"
% possible rename to slurs-?
% TODO: find out what drarn means, and if there's an overlap with drarn.ly
\header{
texidoc="@cindex Drarn Slurs
Slurs can be forced to always attach to note heads.
"
}

fragment = \notes {
  \override Slur  #'direction = #1
  \override Slur  #'attachment = #'(head . head)
  g''16(g)(g)(g)(d')(d)(d)(d)
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { raggedright = ##t}  
}

