\version "1.5.68"

\header{
texidoc="
Volta (Semi folded) behavior.  Voltas can start on non-barline moments.
If they don't barlines should still be shown.
"
}

%  no alts.
\score {
  \notes \context Voice \relative c'' {
    % repeat non aligning with barlines.
    \repeat volta 3 { c^"3$\\times$ 0alt" d e }
    % less alts than body
    \repeat volta 4 { c^"4$\\times$ 2alt" d } \alternative { e f }
    % more alts than body
    \repeat volta 2 { c^"2$\\times$ 3alt" d } \alternative { e f g }
  }
  \paper {}
  \midi {}
}

