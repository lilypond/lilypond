\version "2.1.22"
\header {
    texidoc = "Grace notes and unfolded repeats.
Line breaks may happen before  grace  notes.
"
}
    

\score{
  \notes\context Voice \relative c'{
    \repeat unfold  10 {\grace d8 c4 d e f}

  }
}

