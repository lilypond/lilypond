\version "2.3.16"

\header{
texidoc="
Folded repeat may not make sense without alternatives, and there
should not be more alternatives than repeats.
"
}

\score {
  \context Staff  \relative c'' {
    \repeat fold 3 { c^"3$\\times$ 0alt" d }
    % less alts than body
    \repeat fold 4 { c^"4$\\times$ 2alt" d } \alternative { e f }
    % more alts than body
    \repeat fold 2 { c^"2$\\times$ 3alt" d } \alternative { e f g }
  }
}


