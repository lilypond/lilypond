\version "2.23.14"

\header
{
  texidoc = "A separate @samp{Grace_auto_beam_engraver} initiates
autobeaming at the start of each @code{\\grace} command."
}

\layout { short-indent = 1\cm  indent = 2\cm }

music = {
  \tag top \textMark "manual"
  \relative c'' {
    \grace {a8[ b] } a1 
    \grace {a8[ b c b gis] } a1 
    \grace {a8[ b c32 b c16 gis] } a1
    \grace {a8[ b c16 d] c4 b8[ gis] } a1
    \grace {a8[ b c16 d] c4 b8 a b\fermata a16[ gis] } a1
  } \break
  \tag top \textMark "automatic"
  \relative c'' {
    \grace {a8 b } a1
    \grace {a8 b c b gis } a1
    \grace {a8 b c32 b c16 gis } a1
    \grace {a8 b c16 d c4 b8 gis } a1
    \grace {a8 b c16 d c4 b8 a b\fermata a16 gis } a1
  }
}

\score {
  <<
    \new Staff \with { instrumentName = \markup \center-column
		       { without engraver } }
    \new Voice \with { \remove "Grace_auto_beam_engraver" }
               \music
    \new Staff \with { instrumentName = \markup \center-column
		       { with engraver } }
    \new Voice \removeWithTag top \music
  >>
}
