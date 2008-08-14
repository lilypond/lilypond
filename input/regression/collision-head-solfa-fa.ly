
\header {
  texidoc = "The FA note (a triangle) is merged to avoid creating a
  block-shaped note."
}

\version "2.11.51"

<<
  \key c \major
  \set Staff.shapeNoteStyles = #'#(do re mi fa #f la ti)

  { f'4 }
  \\
  { f'4 }
>>


