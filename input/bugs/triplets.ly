\version "1.3.148"

\header {
texidoc = "Simple beams.  This broke somewhere < 1.3.110
"
  title = "Gammes Chromatiques"
  composer = ""
  filename = "gammes_chromatiques.ly"
}

%{
	    - At bar 3 of 2nd score, stems are too big (or the beams are
          badly positionned)
%}

exI = \notes \relative c' {
  \repeat "volta" 2 {
    \times 2/3 {c8( cis d} \times 2/3 {dis e f} 
    \times 2/3 {fis g gis} \times 2/3 {a bes b} |
    \property Voice.TupletBracket \override #'tuplet-number-visibility = ##f
    \property Voice.TupletBracket \override #'tuplet-bracket-visibility = ##f
    \times 2/3 {c8 cis d} \times 2/3 {dis e f} 
    \times 2/3 {fis g gis} \times 2/3 {a bes b} |
    \times 2/3 {c b bes} \times 2/3 {a aes g}
    \times 2/3 {fis f e} \times 2/3 {ees d des} |
    \times 2/3 {c b bes} \times 2/3 {a aes g}
    \times 2/3 {fis f e} \times 2/3 {ees d )des}
  }
  c1 || \break
}

\score { \context Staff { \notes { \exI } } }
