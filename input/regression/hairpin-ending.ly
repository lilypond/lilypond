
\version "2.3.4"

\header {

texidoc = "Hairpin dynamics start under notes if there are
no text-dynamics. If there are text dynamics, the hairpin does not run
into them."

}


\score {
 \relative c'' { c4 \> c4 c4\! c4_\ff \> c4 c4\!\p }
\paper { raggedright = ##t} 
}

