#(ly:set-option 'old-relative)
\version "1.9.0"
\header{ texidoc="@cindex Embedded Tex
You can embed Tex commands in your score. "}

fragment = \notes {
  a''^"3 $\\times$ \\`a deux"
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { raggedright = ##t }  
}

