
\version "2.12.0"
\header { texidoc = "Each clef has its own accidental placing
rules. "}

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
}



