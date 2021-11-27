\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Section labels appear at the top of the system, appear at
the beginning of a line at a break, remain visible at the end of the
score, and can be styled via the @code{SectionLabel} grob."
}

\layout {
  indent = 0
  ragged-right = ##t
}

\new Score <<
  \new Staff { R1*4 }
  \new Staff {
    \sectionLabel "Ouverture" R1
    \key d \major d'2 \sectionLabel "!.....?" \key c \major g'2
    \break
    \sectionLabel "Mittelteil" \repeat volta 2 { R1 }
    \sectionLabel "Coda" \time 2/2 R1
    \once \override Score.SectionLabel.rotation = #'(-15 0 0)
    \sectionLabel "Kraj"
  }
>>
