
\version "1.9.1"
% possible rename to slurs-?
% TODO: find out what drarn means, and if there's an overlap with drarn.ly
\header{
texidoc="@cindex Drarn Slurs
Slurs can be forced to always attach to note heads.
"
}

fragment = \notes {
  \property Voice.Slur \set #'direction = #1
  \property Voice.Slur \set #'attachment = #'(head . head)
  g''16(g)(g)(g)(d')(d)(d)(d)
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { raggedright = ##t}  
}

