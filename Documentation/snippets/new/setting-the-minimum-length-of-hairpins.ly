\version "2.23.4"

\header {
  lsrtags = "expressive-marks"

  texidoc = "
If hairpins are too short, they can be lengthened by modifying the
@code{minimum-length} property of the @code{Hairpin} object.
"

  doctitle = "Setting the minimum length of hairpins"
}


<<
  {
    \after 4 \< \after 2 \> \after 2. \! f'1
    \override Hairpin.minimum-length = #8
    \after 4 \< \after 2 \> \after 2. \! f'1
  }
  {
    \repeat unfold 8 c'4
  }
>>
