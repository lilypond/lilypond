\version "2.11.57"
\header {

texidoc = "Several automatic accidental rules
aim to reproduce contemporary music notation
practices:
@itemize
@item
'dodecaphonic style prints accidentals on every
note (including naturals)
@item
'neo-modern style prints accidentals on every note
(not including naturals), except when a note is
immediately repeated
@item
'neo-modern-cautionary style acts like neo-modern,
adding cautionary parentheses around accidentals.
@end itemize

Both scores should show the same accidentals.
"

}

\layout { ragged-right = ##t }

\score {
  \relative c'' {
    #(set-accidental-style 'dodecaphonic)
    gis4 a g gisis
    #(set-accidental-style 'neo-modern)
    gis8 a gis gis g' gis gis,, a'
    #(set-accidental-style 'neo-modern-cautionary)
    eis fis eis eis g2
  }
}

\score {
  \relative c'' {
    \set Staff.autoAccidentals = #'()
    \set Staff.autoCautionaries = #'()
    gis!4 a! g! gisis!
    gis!8 a gis! gis g'! gis! gis,,! a'
    eis! fis! eis? eis g?2
  }
}
