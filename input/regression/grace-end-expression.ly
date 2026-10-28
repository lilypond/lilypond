\version "2.27.5"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Grace notes at the end of an expression don't cause crashes."
}

{ e' \acciaccatura << e'8 \\ cis' >> }
