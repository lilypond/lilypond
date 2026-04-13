\version "2.25.35"

\header {
  categories = "Contexts and engravers, Paper and layout, Staff
                notation"

  texidoc = "
In many orchestral scores it is custom to not show staves for
instruments that are silent for a while; this is called a @q{Frenched}
score. LilyPond provides this functionality via the
@code{\\RemoveEmptyStaves} command.

When they play again it is often preferred to show the staves of
@emph{all instruments of such a group}. This can be done by adding the
@code{Keep_alive_together_engraver} to the grouping context (e.g.,
@code{GrandStaff} or @code{StaffGroup}).

In the example below the violins are silent in the second system. Only
the first violin plays the last measure in the third system but the
staff of the second violin is also displayed.
"

  doctitle = "Displaying a whole GrandStaff system if only one of its staves is alive"
} % begin verbatim


\score {
  <<
    \new Staff = "Staff_flute" \with {
      instrumentName = "Flute"
      shortInstrumentName = "Fl"
    } \relative c' {
      \*3 { c'4 c c c | c c c c | c c c c | \break }
    }

    \new StaffGroup = "StaffGroup_Strings" <<
      \new GrandStaff = "GrandStaff_violins" <<
        \new Staff = "StaffViolinI" \with {
          instrumentName = "Violin I"
          shortInstrumentName = "Vi I"
        } \relative c'' {
          a1 | R1*7 | \*12 a16 a4 |
        }
        \new Staff = "StaffViolinII" \with {
          instrumentName = "Violin II"
          shortInstrumentName = "Vi II"
        } \relative c' {
          e1 | R1*8 |
        }
      >>

      \new Staff = "Staff_cello" \with {
        instrumentName = "Cello"
        shortInstrumentName = "Ce"
      } \relative c {
        \clef bass \*9 c1 |
      }
    >>
  >>
}

\layout {
  indent = 3.0\cm
  short-indent = 1.5\cm

  \context {
    \GrandStaff
    \consists Keep_alive_together_engraver
  }
  \context {
    \Staff
    \RemoveEmptyStaves
  }
}
