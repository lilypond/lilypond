\version "2.19.38"
\header {

  texidoc = "@code{make-relative} is a Scheme utility macro mainly
useful for creating music functions accepting pitches as arguments.
Its purpose is to make music functions taking pitch arguments for
producing complex music fragments integrate nicely within a
@code{\\relative} section.  This regtest typesets a short music
fragment twice, once without using @code{\\relative}, once using it.
The fragment should appear identical in both cases."

}

\layout { ragged-right= ##t }

ph =
#(define-music-function (p1 p2 p3 p4 p5)
  (ly:pitch? ly:pitch? ly:pitch? ly:pitch? ly:pitch?)
  (make-relative (p1 p2 p3 p4 p5) (make-event-chord (list p1 p2 p3 p4 p5))
   #{
     \repeat unfold 2 { $p1 2 } |
     \repeat unfold 2 { r16 $p2 8. ~ $p2 4 } |
     \repeat unfold 2 { r8 $p3 16 $p4 $p5 $p3 $p4 $p5 } |
   #}))

\parallelMusic low,middle,high
{
  \ph c' e' g' c'' e''
  R1*7 | \skip 1*7 | \oneVoice R1*7 \voiceOne |
  \ph a c' e' g' c''
  \voiceTwo | \change Staff = "down" \voiceOne | \oneVoice |
  \ph d a d' fis' c''
  \oneVoice R1*21 \voiceTwo | \skip 1*21 | R1*21 |
  \ph c, c g bes e'
  c,2~ 2 | r16 c8. ~ 4 ~ 2
  | r8 f16 a c' f' c' a c' a f a f d f d |
  c,2~ 2 | r16 b,8. ~ 4 ~ 2
  | r8 g'16 b' d'' f'' d'' b' d'' b' g' b' d' f' e' d' |
  c,1\fermata | c1 | <e' g' c''>1\fermata \bar "|." |
}

\score {
  \new PianoStaff
  \compressMMRests <<
    \new Staff = "up" {
      << \high \\ \middle >>
    }
    \new Staff = "down" {
      \clef bass
      \low
    }
  >>
}

\parallelMusic low,middle,high
\relative c' {
  \ph c e g c e
  R1*7 | \skip 1*7 | \oneVoice R1*7 \voiceOne |
  \ph a c e g c
  \voiceTwo | \change Staff = "down" \voiceOne | \oneVoice |
  \ph d, a' d fis c'
  \oneVoice R1*21 \voiceTwo | \skip 1*21 | R1*21 |
  \ph c, c' g' bes e
  c2~ 2 | r16 c'8. ~ 4 ~ 2
  | r8 f16 a c f c' a c a f a f d f d |
  c,,2~ 2 | r16 b'8. ~ 4 ~ 2
  | r8 g'16 b d f d b d b g b d f e d |
  c,,1\fermata | c'1 | <e' g c>1\fermata \bar "|." |
}

\score {
  \new PianoStaff
  \compressMMRests <<
    \new Staff = "up" {
      << \high \\ \middle >>
    }
    \new Staff = "down" {
      \clef bass
      \low
    }
  >>
}
