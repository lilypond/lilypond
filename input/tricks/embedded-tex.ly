
fragment = \notes {
  a''^"3 $\\times$ \\`a deux"
}

\paper { linewidth = -1.; } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
