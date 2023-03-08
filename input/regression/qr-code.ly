\version "2.25.3"

\header {
  texidoc = "The @code{\\qr-code} markup command inserts a QR code,
which can be printed at an arbitrary size.  The padding around it
scales with the size."
}

\markup \box \left-align \qr-code #10 "https://lilypond.org"
\markup \box \left-align \qr-code #20 "https://lilypond.org"
\markup \box \left-align \qr-code #30.0 "https://lilypond.org"
\markup \box \left-align
  \override #'(quiet-zone-size . 6)
  \qr-code #30.0 "https://lilypond.org"

