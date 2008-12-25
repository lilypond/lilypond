
\version "2.12.0"

\header {

  texidoc = "Hairpin dynamics start under notes if there are
no text-dynamics. If there are text dynamics, the hairpin does not run
into them."

}

\layout { ragged-right = ##t } 

\relative c'' {
  c4 \> c4 c4\! c4_\ff \> c4 c4\!\p
  
  <<
    { c\< c\! }
    \lyrics { "a" loooong }
  >>
}

