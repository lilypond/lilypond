
\version "2.19.56"

\header {
  lsrtags = "headword"

  texidoc = "
Wind headword

"
  doctitle = "Wind headword"
}

%% http://lsr.di.unimi.it/LSR/Item?id=833
%% see also https://lilypond.org/doc/v2.18/Documentation/notation/wind-instruments

% NR 2.whatever  Wind
% Tchaikovsky
% Nutcracker Suite, VII Dance of the Merlitons

#(set-global-staff-size 15)

\score {
  \new StaffGroup <<
    \new Staff \with { instrumentName = "Flauto I,II" }
    \relative c'' {
      \once \override Score.RehearsalMark.self-alignment-X = #-1
      \once \override Score.RehearsalMark.break-align-symbols = #'(time-signature)
      \once \override Score.TimeSignature.break-align-anchor-alignment = #LEFT
      \once \override Score.RehearsalMark.padding = #4
      \mark \markup \large \bold {Moderato assai}
      \key d \major
      \time 2/4
      \compressMMRests R2*2
      <d a>16-.\p <cis g>-. <d a>-. <cis g>-. <d a>8-. <cis g>-.
      <e a,>-.\< <d a>32( <fis d> <a fis> <d a> <fis d>4--)\mf
      <g d>16-. <fis cis>-. <g d>-. <fis cis>-.
      <e b>(\> <d a>) <a fis>-. <fis d>-.\!
      <d bes>4--\sf \acciaccatura {<d' bes>8} <cis a>4--\mf
    }
    \new Staff \with { instrumentName = "Flauto III" }
    \relative c' {
      \key d \major
      \time 2/4
      \compressMMRests R2*2_\markup{Gr.Fl.}
      fis16-.\p e-. fis-. e-. fis8-. e-.
      g8-.\< fis32( a d fis a4--)\mf
      b16-. a-. b-. a-. g(\> fis) d-. a-.\!
      g4--\sf \acciaccatura fis'8 g4--\mf
    }
  >>
}
