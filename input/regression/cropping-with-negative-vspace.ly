\version "2.25.81"

\header {
  texidoc = "Test case for cropping with negative vspace.

This test ensures that when cropping is enabled, the bounding box correctly
includes content that has been shifted upwards using negative vspace."
}

\markup \column {
  top
  \vspace #-2
  bottom
}
