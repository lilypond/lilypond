\version "2.19.21"

\header {
	texidoc = "addlyrics do not need braces around their arguments,
in particular if the arguments are variables."
}

m = \relative { c'4 d }
l = \lyricmode { A B }

% addlyrics takes music expressions as well as music identifiers. The following
% two staves should produce the same output:
\new Staff { {\m} \addlyrics {\l} }
\new Staff { \m \addlyrics \l }
