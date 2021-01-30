\version "2.16.0"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "conflict with event: `%s'") "key-change-event")
#(ly:expect-warning (ly:translate-cpp-warning-scheme "discarding event: `%s'") "key-change-event")

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
