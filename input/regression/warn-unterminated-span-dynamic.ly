\version "2.16.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "unterminated %s") "crescendo")
#(ly:expect-warning (ly:translate-cpp-warning-scheme "unterminated %s") "decrescendo")

\header {
  texidoc = "A warning is printed if a dynamic spanner is
unterminated."
}

<<
  \new Staff {
    % warning expected: unterminated crescendo
    c'1\<
  }
  \new Staff {
    % warning expected: unterminated decrescendo
    c'1\>
  }
>>

