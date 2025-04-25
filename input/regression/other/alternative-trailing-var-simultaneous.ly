\version "2.25.26"

\header {
  texidoc = "It is an error to use @code{\\alternative \\var} following the body
of a repeat when @var{var} holds any type of music other than sequential music
(in this case, simultaneous music)."
}

expect-error = ##t

sim = << e'1 g'1 >>
\repeat unfold 2 c'1 \alternative \sim
