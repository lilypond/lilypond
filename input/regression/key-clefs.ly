
\version "2.17.6"
\header { texidoc = "Each clef has its own accidental placing
rules, which can be adjusted using @code{sharp-positions}
and @code{flat-positions}." }

#(set-global-staff-size 16)


\relative cis' {

				% \clef french % same as octaviated bass
  \clef violin
  \key cis \major cis1  \key ces \major ces
  \clef soprano
  \key cis \major cis \key ces \major ces \break
  \clef mezzosoprano
  \key cis \major cis \key ces \major ces
  \clef alto
  \key cis \major cis \break \key ces \major ces 
  \clef tenor
  \key cis \major cis \key ces \major ces \break
  \clef baritone
  \key cis \major cis \key ces \major ces
  \clef bass
  \key cis \major cis \key ces \major  ces
  \break R1
  \override Score.RehearsalMark.self-alignment-X = #LEFT
  \override Score.RehearsalMark.font-size = #0
  \mark "B-sharp on top"
  \override Staff.KeySignature.sharp-positions = #'(6 0 1 2 3 4 5)
  \override Staff.KeyCancellation.sharp-positions = #'(6 0 1 2 3 4 5)
  \key cis \major R
  \mark "Flats throughout the staff"
  \override Staff.KeySignature.flat-positions = #'((-5 . 5))
  \override Staff.KeyCancellation.flat-positions = #'((-5 . 5))
  \key ces \major R
  \clef tenor
  \key cis \major cis \break \key ces \major ces
  \clef treble
  \key cis \major cis \key ces \major ces
}

