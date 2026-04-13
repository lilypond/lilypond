\version "2.25.35"

\header {
  texidoc = "A user-defined empty bar line with an annotation in the
name behaves like the built-in empty bar line.  The horizontal space
between notes should be the same in both measures."
}

\layout {
  ragged-right = ##t
}

%% This definition is intended to match the definition of "" in
%% scm/bar-line.scm except for the annotation in the name.
\defineBarLine "-ann" #'(#t #f #f)

\new Score \fixed c' {
  \*3 { c4 \bar "" } c
  \*3 { c4 \bar "-ann" } c
}
