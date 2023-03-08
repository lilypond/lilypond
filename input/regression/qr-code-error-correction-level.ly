\version "2.25.3"

\header {
  texidoc = "QR codes can be created with all four possible
error correction levels."
}

#(ly:set-option 'warning-as-error)

\markup \qr-code #10 "https://lilypond.org"
\markup \override #'(error-correction-level . low) % default
  \qr-code #10 "https://lilypond.org"
\markup \override #'(error-correction-level . medium)
  \qr-code #10 "https://lilypond.org"
\markup \override #'(error-correction-level . quarter)
  \qr-code #10 "https://lilypond.org"
\markup \override #'(error-correction-level . high)
  \qr-code #10 "https://lilypond.org"

% Gentle error for an invalid level
#(ly:expect-warning
  (G_ "QR code error correction level must be 'low, 'medium, 'quarter \
or 'high; found ~s")
  'invalid)
\markup \override #'(error-correction-level . invalid)
  \qr-code #10 "https://lilypond.org"

