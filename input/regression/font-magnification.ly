\header { texidoc = "The magnification can be set for any font. Note
that this doesn't change variable symbols such as beams or slurs. " }

\score {
\notes \relative c'' \context Voice {
\property Voice .NoteHead \set #'font-magnification = #0.9
c4
\property Voice .NoteHead \set #'font-magnification = #0.8


  % why doesn't this  work?
  c4-#'((font-series . bold) ((font-magnification . 2.0) ("foobar")))

  \property Voice .NoteHead \set #'font-magnification = #1.2
  \property Voice.TextScript \set #'font-magnification = #2.0
  c4-"big"
  \property Voice .NoteHead \set #'font-magnification = #1.6
  c4
}

}

