\version "1.3.146"

\header {

texidoc = "Hairpin dynamics start under notes if there are
no text-dynamics. If there are text dynamics, the hairpin does not run
into them."

}


\score {
\notes \relative c'' { c4 \> c4 \! c4 c4_\ff \> c4 \! c4 \p }
\paper { linewidth = -1.} 
}
