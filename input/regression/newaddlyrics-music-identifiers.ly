\version "2.16.0"

\header {
	texidoc = "addlyrics do not need braces around their arguments,
in particular if the arguments are variables."
}

m = \relative c' { c4 d }
l = \lyricmode { A B }

% addlyrics takes music expressions as well as music identifiers. The following
% two staves should produce the same output:
\new Staff { {\m} \addlyrics {\l} }
\new Staff { \m \addlyrics \l }
