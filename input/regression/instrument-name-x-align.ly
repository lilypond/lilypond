\header {

  texidoc = "Instrument names horizontal alignment is tweaked by
changing the @code{Staff.InstrumentName.self-alignment-X} property.  The
@code{\\layout} variables @code{indent} and @code{short-indent} define
the space where the instrument names are aligned before the first and
the following systems, respectively."

}

\version "2.17.6"
\paper { left-margin = 3\cm }
\score {
  \new StaffGroup <<
    \new Staff {
      \override Staff.InstrumentName.self-alignment-X = #LEFT
      \set Staff . instrumentName = \markup \left-column {
	"Left aligned" "instrument name"
      }
      \set Staff . shortInstrumentName = "Left"
      c''1 \break c''
    }
    \new Staff {
      \override Staff.InstrumentName.self-alignment-X = #CENTER
      \set Staff . instrumentName = \markup \center-column {
	"Centered" "instrument name"
      }
      \set Staff . shortInstrumentName = "Centered"
      g'1 g'
    }
    \new Staff {
      \override Staff.InstrumentName.self-alignment-X = #RIGHT
      \set Staff . instrumentName = \markup \right-column {
	"Right aligned" "instrument name"
      }
      \set Staff . shortInstrumentName = "Right"
      e' \break e'
    }
  >>
  \layout {
    ragged-right = ##t
    indent = 4\cm
    short-indent = 2\cm
  }
}
