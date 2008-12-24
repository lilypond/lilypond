\version "2.12.0"

\header {
  texidoc = "Text set in TrueType Fonts that contain kerning tables, are kerned. "
}

\markup {
%  \override #'(font-name . "VeraSerif")
  \override #'(padding . 0)
  \override #'(thickness . 0.00001)
  \override #'(box-padding . 0.0)
  \column {
    \line { \pad-to-box #'(0 . 50) #'(0 . 2) \line { With kerning: }
	    \override #'(font-size . 12)
	    \box { VAVAVA } }
    \line {
      \pad-to-box #'(0 . 50) #'(0 . 2) \line { Without kerning: }
      \override #'(font-size . 12)
      \override #'(word-space . 0.0) \line { \box V \box A \box V \box A \box V \box A } }
  }
}

