
\version "2.3.8"
\header{ texidoc="@cindex Embedded Tex
You can embed Tex commands in your score. "}

fragment =  {
  a''^"3 $\\times$ \\`a deux"
}

\paper { raggedright = ##t} 

\score {
  \relative c \fragment
  \paper { raggedright = ##t }  
}

