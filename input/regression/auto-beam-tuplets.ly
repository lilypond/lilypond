#(ly:set-option 'old-relative)
\version "1.9.0"

\header {
texidoc = "Tuplet-spanner should not put (visible) brackets on
beams even if they're auto generated."
}

\score { \notes \relative c' {
  \property Voice.tupletSpannerDuration = #(ly:make-moment 1 4)
  \property Voice.TupletBracket \override #'bracket-visibility = #'if-no-beam
  \times 2/3 {
	 f8-[ f f ] f8-[ f f ] f f f f f f 
 }
}
    \paper { raggedright= ##t }

     }
