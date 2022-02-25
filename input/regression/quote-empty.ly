\version "2.21.7"

\header {
  texidoc = "A warning should be produced for empty quoted music."
}

#(ly:expect-warning (G_ "quoted music `~a' is empty") 'test)

test = {}
\addQuote "test" \test

\markup "This space intentionally left blank."
