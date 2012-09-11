
\header {
  texidoc = "The FA note (a triangle) is merged to avoid creating a
  block-shaped note."
}

\version "2.16.0"

{
  \key c \major
  \set Staff.shapeNoteStyles = #'#(do re mi fa #f la ti)
  <<
    { f'4 }
    \\
    { f'4 }
  >>
  \set Staff.shapeNoteStyles = #'#(do re mi faThin #f la ti)
  <<
    { f'4 }
    \\
    { f'4 }
  >>
}


