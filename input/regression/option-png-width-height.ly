\version "2.21.5"

\header {
  texidoc = "Test functionality of the @option{-dpng-width} and
             @option{-dpng-height} command line options.  Affects
             PNG output only."
}

#(ly:set-option 'png-width 500)
#(ly:set-option 'png-height 300)

{ c'' }
