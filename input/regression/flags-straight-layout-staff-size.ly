\version "2.23.14"

\header {
  texidoc = "Straight flags scale according to @code{layout-set-staff-size} in
@code{MetronomeMark}, @code{TextScript} and music."
}

\layout { \markLengthOn }

note = { \autoBeamOff \tempo 16 = 60 b16_\markup \note-by-number #4 #0 #-1 }

mus = {
  \note
  \override Score.MetronomeMark.flag-style = #'modern-straight-flag
  \override TextScript.flag-style = #'modern-straight-flag
  \override Flag.stencil = #modern-straight-flag
  \note
  \override Score.MetronomeMark.flag-style = #'old-straight-flag
  \override TextScript.flag-style = #'old-straight-flag
  \override Flag.stencil = #old-straight-flag
  \note
  \override Score.MetronomeMark.flag-style = #'flat-flag
  \override TextScript.flag-style = #'flat-flag
  \override Flag.stencil = #flat-flag
  \note
}

\score { \mus \layout { #(layout-set-staff-size 6) } }

\score { \mus }

\score { \mus \layout { #(layout-set-staff-size 60) } }
