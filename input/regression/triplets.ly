\version "1.5.68"

\header {
texidoc="Simple beams.  This broke somewhere < 1.3.110

DOCME ! what is this. 

"
  title = "Gammes Chromatiques"
  composer = ""
  filename = "gammes_chromatiques.ly"
}

ex = \notes \relative c' {
  \repeat "volta" 2 {
    \times 2/3 {c8( cis d} \times 2/3 {dis e f} 
    \times 2/3 {fis g gis} \times 2/3 {a bes b} |
    \property Voice.TupletBracket \override #'number-visibility = ##f
    \property Voice.TupletBracket \override #'bracket-visibility = ##f
    \times 2/3 {c8 cis d} \times 2/3 {dis e f} 
    \times 2/3 {fis g gis} \times 2/3 {a bes b} |
    \times 2/3 {c b bes} \times 2/3 {a aes g}
    \times 2/3 {fis f e} \times 2/3 {ees d des} |
    \times 2/3 {c b bes} \times 2/3 {a aes g}
    \times 2/3 {fis f e} \times 2/3 {ees d )des}
  }
  c1 
}

\score { \context Staff { \notes { \ex } } }
