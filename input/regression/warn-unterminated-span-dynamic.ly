\version "2.13.4"

#(ly:set-option 'warning-as-error #f)

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

