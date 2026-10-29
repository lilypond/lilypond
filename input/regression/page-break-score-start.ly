\version "2.27.5"

\header { texidoc="Warn about dead pagebreak at beginning of second score." }

\layout { ragged-right = ##t }

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "Ignoring page break at score start"))

{
  s1
}

{
  \pageBreak
  \repeat unfold 19 c'1
}
