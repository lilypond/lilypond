\version "2.16.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "Grace notes at the end of an expression don't cause crashes."
}

{ e' \acciaccatura << e'8 \\ cis' >> }
