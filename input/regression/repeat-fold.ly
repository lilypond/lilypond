\version "2.7.39"

\header{
texidoc="
Folded repeat may not make sense without alternatives, and there
should not be more alternatives than repeats.
"
}

\paper { ragged-right = ##t } 

\context Staff  \relative c'' {
    \repeat fold 3 { c^"3x 0alt" d }
    % less alts than body
    \repeat fold 4 { c^"4x 2alt" d } \alternative { e f }
    % more alts than body
    \repeat fold 2 { c^"2x 3alt" d } \alternative { e f g }
  }


