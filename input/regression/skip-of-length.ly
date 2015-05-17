\header {
  texidoc = "skip-of-length and mmrest-of-length create skips and rests that
last as long as their arguments."
  
}
\paper {
  ragged-right = ##T
}

\version "2.19.21"

\relative
<<
  \new Staff {
    c'\breve    f4 r2.
    c\breve    f4 r2.
    s\breve^"skip"
  }
  \new Staff {
    \applyMusic #skip-of-length { c\breve } f4 r2.
    \applyMusic #mmrest-of-length { c\breve } f4 r2.
    \musicMap #skip->rest s\breve 
  }
>>
  
