 ﻿\version "2.15.9"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "This input file contains a UTF-8 BOM not at the very beginning,
  but on the first line after the first byte. LilyPond should gracefully
  ignore this BOM as specified in RFC 3629, but print a warning."
}

{ c }
