\version "2.14.0"
#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "Two simultaneous %s events, junking this one") "key-change")
#(ly:expect-warning (ly:translate-cpp-warning-scheme "Previous %s event here") "key-change")

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
