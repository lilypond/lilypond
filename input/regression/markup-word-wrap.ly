\header {

  texidoc = "The markup commands @code{\\wordwrap} and @code{\\justify}
  produce simple paragraph text."

}

\version "2.21.0"

\markup {
  this is normal  text
  \override #'(line-width . 60)
  \wordwrap {
    This is a test of the wordwrapping function.
    1 This is a test of the wordwrapping function.
    2 This is a test of the wordwrapping function.
    3 This is a test of the wordwrapping function.
    4 1a111 11111  \bold 22222 \italic  2222
  }
  continuing
}

\markup {
  this is normal  text
  \override #'(line-width . 40)
  \justify {
    This is a test of the wordwrapping function, but with justification.
    1 This is a test of the wordwrapping function, but with justification.
    2 This is a test of \fraction a b the wordwrapping function, but with justification.
    3 This is a test of the wordwrapping function, but with justification. bla bla
  }
  continuing
}


\markup {

  \override #'(line-width . 40)

{  \wordwrap-string " Om mani padme hum Om mani padme hum Om mani
padme hum Om mani padme hum Om mani padme hum Om mani padme hum Om
mani padme hum Om mani padme hum.

Gate Gate paragate Gate Gate paragate Gate Gate paragate Gate Gate
paragate Gate Gate paragate Gate Gate paragate."

  \justify-string " Om mani padme hum Om mani padme hum Om mani
padme hum Om mani padme hum Om mani padme hum Om mani padme hum Om
mani padme hum Om mani padme hum.

Gate Gate paragate Gate Gate paragate Gate Gate paragate Gate Gate
paragate Gate Gate paragate Gate Gate paragate." }

}

% Test different line endings (LF vs. CRLF):
\markup {
  \override #'(line-width . 40)
  {
    % normal unix lineendings
    #(make-justify-string-markup " Om mani padme hum Om mani padme hum Om mani\npadme hum Om mani padme hum Om mani padme hum Om mani padme hum Om\nmani padme hum Om mani padme hum.\n\nGate Gate paragate Gate Gate paragate Gate Gate paragate Gate Gate\nparagate Gate Gate paragate Gate Gate paragate.")

    % use Windows CRLF and get the same result
    #(make-justify-string-markup " Om mani padme hum Om mani padme hum Om mani\r\npadme hum Om mani padme hum Om mani padme hum Om mani padme hum Om\r\nmani padme hum Om mani padme hum.\r\n\r\nGate Gate paragate Gate Gate paragate Gate Gate paragate Gate Gate\r\nparagate Gate Gate paragate Gate Gate paragate.")
  }
}