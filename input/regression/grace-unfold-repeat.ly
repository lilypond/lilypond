\header {
    texidoc = "Grace notes and unfolded repeats.
Line breaks may happen before  grace  notes.
"
}
    

\score{
  \notes\context Voice \relative c'{

    % Bug 2: What happens with the grace notes here?
    \repeat unfold  10 {\grace d8 c4 d e f}

  }
}
