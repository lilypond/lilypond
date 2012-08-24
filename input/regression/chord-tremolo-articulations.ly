\version "2.16.0"

\header{
texidoc="
Articulations on chord tremolos should not confuse the time-scaling of the
notes. In particular, only the number of real notes should be considered.
"
}

\context Voice \relative c' {
  \repeat "tremolo" 4 { d16\f e-. }
  \repeat "tremolo" 4 { d16-> e } | \barNumberCheck #2
  \repeat "tremolo" 4 { d16 e\f }
  \repeat "tremolo" 8 { d32\> e\! } | \barNumberCheck #3
  \repeat "tremolo" 2 { d8\trill e }
  \repeat "tremolo" 2 { d8\sfz e } | \barNumberCheck #4

  \time 2/4
  \repeat "tremolo" 8 { d32^"Markup" e } | \barNumberCheck #5
  c4 c4
}

