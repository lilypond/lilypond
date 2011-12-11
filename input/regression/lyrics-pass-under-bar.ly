\version "2.15.22"

\header {
  texidoc = "Long lyrics should be allowed to pass under
the bar line.
"
}

\relative c''' { c2 c c c }
\addlyrics { foo bar foooooooo bar }
