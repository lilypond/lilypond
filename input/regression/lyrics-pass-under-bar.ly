\version "2.16.0"

\header {
  texidoc = "Long lyrics should be allowed to pass under
the bar line.
"
}

\relative c''' { c2 c c c }
\addlyrics { foo bar foooooooo bar }
