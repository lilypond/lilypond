\version "1.3.146"

\header{
texidoc="
Folded.  This doesn't make sense without alternatives, but it works.
"
}

\score {
  \context Staff \notes \relative c'' {
    \repeat fold 3 { c^"3$\\times$ 0alt" d }
    % less alts than body
    \repeat fold 4 { c^"4$\\times$ 2alt" d } \alternative { e f }
    % more alts than body
    \repeat fold 2 { c^"2$\\times$ 3alt" d } \alternative { e f g }
  }
}

