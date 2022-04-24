\version "2.19.48"

% Engrave to png with these options to create
% the large version of image for home page:
% -dno-point-and-click --png -dresolution=300

% The small version you see on the home page
% itself is simply the image from the essay manual,
% downloaded from the website version of the
% essay manual.

\layout {
  line-width = 15.9 \cm
}

\paper {
  indent = 0
  paper-height = 8 \cm
  paper-width = 18 \cm
}

\header {
  tagline = ##f
}

#(set-global-staff-size 14.3)


global = { \key g \minor }

partI = \relative {
  \voiceOne
  fis'8 d' ees g, fis4 g
  r8 a16 bes c8 bes16 a d8 r r4
  r2 r8 d16 ees f8 ees16 d
  ees4 ~ 16 d c bes a4 r8 ees'16 d
  c8 d16 ees d8 e16 fis g8 fis16 g a4 ~
  8 d, g f ees d c bes
  a2 g \fermata \bar "|."
}

partII = \relative {
  \voiceTwo
  d'4 r4 r8 d'16 c bes8 c16 d
  ees8 d c ees a, r r4
  r8 fis16 g a8 g16 fis g2 ~
  2 r8 d' ees g,
  fis4 g r8 a16 bes c8 bes16 a
  bes4. <g b>8 <a c> r <d, g> r
  <ees g>4 <d fis> d2
}

partIII = \relative {
  \voiceOne
  r2 r8 d' ees g, fis4 g r8 a16 bes c8 bes16 a
  bes2 ~ 8 b16 a g8 a16 b
  c4 r r2
  R1
  r8 d ees g, fis4 g
  r8 a16 bes c8 bes16 a b2
}

partIV = \relative {
  \voiceTwo
  d4 r r2
  r8 d ees g, fis4 a
  d,8 d'16 c bes8 c16 d ees2 ~
  8 ees16 d c8 d16 ees fis,8 a16 g fis8 g16 a
  d,8 d'16 c bes8 c16 d ees8 c a fis'
  g f ees d c bes a g
  c a d d, g2 \fermata
}

\score {
  <<
    % \set Score.barNumberVisibility = #all-bar-numbers-visible
    % required in 2.13
    \set Score.currentBarNumber = #28
    \bar ""
    \new PianoStaff <<
      \new Staff = "RH" <<
        \global
        \new Voice = "voiceI" { \partI }
        \new Voice = "voiceII" { \partII }
      >>

      \new Staff = "LH" <<
        \clef "bass"
        \global
        \new Voice = "voiceIII" { \partIII }
        \new Voice = "voiceIV" { \partIV }
      >>
    >>
  >>
  \layout {
    \context {
      \Staff
      \remove Time_signature_engraver
    }
    \context {
      \PianoStaff
      \override StaffGrouper.staff-staff-spacing.padding = #1
    }
  }
}
