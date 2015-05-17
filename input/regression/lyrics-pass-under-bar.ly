\version "2.19.21"

\header {
  texidoc = "Long lyrics should be allowed to pass under
the bar line.
"
}

\relative { c'''2 c c c }
\addlyrics { foo bar foooooooo bar }
