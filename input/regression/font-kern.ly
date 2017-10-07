\version "2.21.0"

\header {
  texidoc = "Text set in TrueType Fonts that contain kerning tables, are kerned. "
}

\markup {
%  \override #'(font-name . "VeraSerif")
  \override #'((padding . 0) (thickness . 0.00001) (box-padding . 0.0))
  \column {
    \line { \pad-to-box #'(0 . 50) #'(0 . 2) \line { With kerning: }
	    \override #'(font-size . 12)
	    \box { VAVAVA } }
    \line {
      \pad-to-box #'(0 . 50) #'(0 . 2) \line { Without kerning: }
      \override #'((font-size . 12) (word-space . 0.0))
      \line { \box V \box A \box V \box A \box V \box A } }
  }
}

