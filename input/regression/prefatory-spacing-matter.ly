
\version "2.1.26"
\header {

    texidoc = "Distances between prefatory items (e.g. clef, bar, etc.)
   are determined by engraving standards.  These distances
   depend on which items are combined."

}

\score { \notes \relative c'' {
	\set Staff.instrument = "fobar"
	\bar "||:"
	\key cis \major
	cis4 cis4 cis4 cis4 \clef bass  cis,1
	\clef treble
	\bar ":|"
}
\paper  { raggedright = ##t}
}

