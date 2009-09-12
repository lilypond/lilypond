\version "2.12.3"

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

