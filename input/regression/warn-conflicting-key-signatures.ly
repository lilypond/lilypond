\version "2.12.0"

\header {
  texidoc = "If you specify two different key signatures at one point, a
warning is printed."

}

\score { 
\context Voice <<
 { \key cis \major cis4 \key bes \major bes4 }
 { \key cis \major fis4 \key es \major g4 }
>>
}
