
\version "2.7.13"

\header {

  texidoc = "Hairpin dynamics start under notes if there are
no text-dynamics. If there are text dynamics, the hairpin does not run
into them."

}

\layout { raggedright = ##t } 

\relative c'' {
  c4 \> c4 c4\! c4_\ff \> c4 c4\!\p
  
  <<
    { c\< c\! }
    \lyrics { "a" loooong }
  >>
}

